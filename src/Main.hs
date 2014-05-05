{-# LANGUAGE FlexibleInstances #-}

import Language.C

main :: IO ()
main = putStrLn "Hello, world!"

parseFromString :: String -> CTranslUnit
parseFromString s = case (parseC (inputStreamFromString s) (nopos)) of
  Left err -> error $ show err
  Right tu -> tu

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

unrollForLoop :: Int -> CStat -> CStat
unrollForLoop n (CFor init until update stmt node) = undefined
unrollForLoop _ _ = error "Non-for-loop given to unrollForLoop."

-- TODO: "do while" case
unrollWhileLoop :: Int -> CStat -> CStat
unrollWhileLoop n (CWhile expr stmt isDoWhile _)
  | not isDoWhile =
    let ni = undefNode
        unrolledStmts = unrollLoops n stmt
        blocks = take n (fmap (\s -> CBlockStmt (CIf expr s Nothing ni)) $ repeat unrolledStmts)
    in CCompound [] blocks ni
  | otherwise = error "TODO: \"do while\" case"
unrollWhileLoop _ _ = error "Non-for-loop given to unrollForLoop."

class Unrollable a where
  unrollLoops :: Int -> a -> a

instance Unrollable CTranslUnit where
  unrollLoops n (CTranslUnit exdecls node) = CTranslUnit (fmap (unrollLoops n) exdecls) node

instance Unrollable CExtDecl where
  unrollLoops n (CFDefExt (CFunDef dss declr decls stmt node)) =
    (CFDefExt (CFunDef dss declr decls (unrollLoops n stmt) node))
  unrollLoops n decl = decl

instance Unrollable CStat where
  unrollLoops n (CCompound ids blocks node) = CCompound ids (fmap (unrollLoops n) blocks) node
  unrollLoops n (CWhile expr stmt isDoWhile node) = unrollWhileLoop n (CWhile expr stmt isDoWhile node)
  unrollLoops n (CFor init until update stmt node) = unrollForLoop n (CFor init until update stmt node)
  unrollLoops n (CSwitch expr stmt node) = CSwitch expr (unrollLoops n stmt) node
  unrollLoops n (CCase expr stmt node) = CCase expr (unrollLoops n stmt) node
  unrollLoops n (CCases expr expr2 stmt node) = CCases expr expr2 (unrollLoops n stmt) node
  unrollLoops n (CDefault stmt node) = CDefault (unrollLoops n stmt) node
  unrollLoops n (CIf expr thenStmt elseStmt node) = CIf expr (unrollLoops n thenStmt) (fmap (unrollLoops n) elseStmt) node
  -- TODO: do we need one for label?
  unrollLoops _ stat = stat

instance Unrollable CBlockItem where
  unrollLoops n (CBlockStmt stmt) = CBlockStmt (unrollLoops n stmt)
  unrollLoops _ (CBlockDecl decl) = CBlockDecl decl
  unrollLoops _ (CNestedFunDef _) = error "Nested function definitions are not supported!"
