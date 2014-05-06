{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

import System.Environment (getArgs)
import System.Console.GetOpt
import Language.C
import Data.Generics

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = getOpt RequireOrder options args
  putStrLn $ show nonOpts

options =
  [
--    Option ['r'] ["numUnroll"] (OptArg )
  ]

-- Loopy programs:
loop1 = parseFromString $
        "int main() {" ++
        "  float x = 1.0f;" ++
        "  while (true) {" ++
        "    int y = x*x;" ++
        "    x += 1.2 * y;"++
        "}" ++
        "  return x;" ++
        "}"

loop2 = parseFromString $
        "int main() {" ++
        "  float x = 1.0f;" ++
        "  while (true)" ++
        "    x += 1.2 * x;"++
        "  return x;" ++
        "}"

multiDecl = parseFromString $
            "int main () {" ++
            "  float x;" ++
            "  x = 3 * 9;" ++
            "  float y;" ++
            "  y += x + 3;" ++
            "  return 0;" ++
            "}";

complicatedDecl = parseFromString $
                  "int main () {" ++
                  "  float a, b=2.0, *c, *d = 4;" ++
                  "  static float n = 3, m;" ++
                  "  return 0;" ++
                  "}";
            
unrollLoops :: Int -> CTranslUnit -> CTranslUnit
unrollLoops n = everywhere' (mkT unrollLoops')
  where 
    unrollLoops' :: CStat -> CStat
    unrollLoops' (CWhile expr stmt isDoWhile _)
      | not isDoWhile =
        let ni = undefNode
            blocks = take n (fmap (\s -> CBlockStmt (CIf expr s Nothing ni)) $ repeat stmt)
        in CCompound [] blocks ni
      | otherwise = error "TODO: \"do {} while\" case"
    unrollLoops' (CFor init until update stmt node) = undefined
    unrollLoops' s = s

removeAssnOps :: CTranslUnit -> CTranslUnit
removeAssnOps = everywhere' (mkT removeAssnOps')
  where
    removeAssnOps' :: CExpr -> CExpr
    removeAssnOps' (CAssign op exprL exprR node) =
      (CAssign CAssignOp exprL exprR' node)
      where
        exprR' =
          case op of
            CAssignOp -> exprR -- Keep regular assignments as they are.
            CMulAssOp -> CBinary CMulOp exprL exprR undefNode
            CDivAssOp -> CBinary CDivOp exprL exprR undefNode
            CRmdAssOp -> CBinary CRmdOp exprL exprR undefNode
            CAddAssOp -> CBinary CAddOp exprL exprR undefNode
            CSubAssOp -> CBinary CSubOp exprL exprR undefNode
            CShlAssOp -> CBinary CShlOp exprL exprR undefNode
            CShrAssOp -> CBinary CShrOp exprL exprR undefNode
            CAndAssOp -> CBinary CAndOp exprL exprR undefNode
            CXorAssOp -> CBinary CXorOp exprL exprR undefNode
            COrAssOp -> CBinary COrOp exprL exprR undefNode
    removeAssnOps' e = e

-- TODO: Make sure relative order is maintained. Idea: use CCompound instead of lists
moveDeclsToTop :: CTranslUnit -> CTranslUnit
moveDeclsToTop = everywhere (mkT moveDeclsToTop')
  where
    moveDeclsToTop' :: [CBlockItem]  -> [CBlockItem]
    moveDeclsToTop' = moveDeclsToTop'' [] []
    moveDeclsToTop'' ds nds (d@(CBlockDecl _):bs) = moveDeclsToTop'' (d:ds) nds bs
    moveDeclsToTop'' ds nds (nd:bs) = moveDeclsToTop'' ds (nd:nds) bs
    moveDeclsToTop'' ds nds [] = ds ++ nds

splitDeclsAndAssn :: CTranslUnit -> CTranslUnit
splitDeclsAndAssn = everywhere (mkT splitDeclsAndAssn')
  where
    splitDeclsAndAssn' :: [CBlockItem] -> [CBlockItem]
    splitDeclsAndAssn' = concatMap splitDecl
    splitDecl bd@(CBlockDecl (CDecl declspecs decls a)) = concatMap splitup decls
      where
        splitup (declr@(Just declr'), i@(Just (CInitExpr initialier _)), expr) =
          [CBlockDecl (CDecl declspecs [(declr, Nothing, expr)] undefNode),
           CBlockStmt (CExpr
                       (Just (CAssign CAssignOp (d2e declr') initialier undefNode))
                       undefNode)]
        -- initialization lists not supported (yet?):
        splitup (declr@(Just declr'), i@(Just _), expr) =
          error "Initialization lists not supported"
        splitup di = [CBlockDecl (CDecl declspecs [di] undefNode)]
    splitDecl bi = [bi]
    d2e (CDeclr (Just i) _ _ _ _) = CVar i undefNode


parseFromString :: String -> CTranslUnit
parseFromString s =
  case (parseC (inputStreamFromString s) (nopos)) of
    Left err -> error $ show err
    Right tu -> tu

parseStmtFromString :: String -> CStat
parseStmtFromString s =
  case (execParser statementP (inputStreamFromString s) (nopos) [] newNameSupply) of
    Left err -> error $ show err
    Right (tu, _) -> tu
