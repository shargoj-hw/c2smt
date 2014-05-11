module AstTransformers where

import Language.C
import Data.Generics

-- For playing in the REPL
import CExamples

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
    unrollLoops' (CFor init until update stmt node) = undefined init until update stmt node
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
    splitDecl bd@(CBlockDecl (CDecl declspecs decls _)) = concatMap splitup decls
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

singleReturnify :: CTranslUnit -> CTranslUnit
singleReturnify = everywhere (mkT returnifyFunction)
  where
    returnifyFunction :: CFunDef -> CFunDef
    returnifyFunction (CFunDef specs decl params body _) =
      let CCompound localLabels blockItems _ = body
          body' = CCompound localLabels (returnifyBody specs blockItems) undefNode
      in CFunDef specs decl params body' undefNode
    returnifyBody :: [CDeclSpec] -> [CBlockItem] -> [CBlockItem]
    returnifyBody retType body = declRetval ++ updatedBlocks ++ newReturn
      where
        retId = internalIdent "GENERATEDRETVAL" 
        retValDecl = CDeclr (Just $ retId) [] Nothing [] undefNode
        declRetval = [CBlockDecl $ CDecl retType [(Just retValDecl, Nothing, Nothing)] undefNode]
        updatedBlocks = everywhere (mkT returnToRetval) body
        newReturn = [CBlockStmt $ CReturn (Just $ CVar retId undefNode) undefNode]
        returnToRetval :: CStat -> CStat
        returnToRetval (CReturn (Just v) _) =
          CExpr (Just $ CAssign CAssignOp (CVar retId undefNode) v undefNode) undefNode
        returnToRetval s = s

simplifyControlFlow :: CTranslUnit -> CTranslUnit
simplifyControlFlow = everywhere (mkT simplifyControlFlowFunc)
  where 
    simplifyControlFlowFunc :: CFunDef -> CFunDef
    simplifyControlFlowFunc (CFunDef specs decl params (CCompound l b _) _) =
      (CFunDef specs decl params (CCompound l (simplifyControlFlowBlocks b []) undefNode) undefNode)
    simplifyControlFlowBlocks :: [CBlockItem] -> [CBlockItem] -> [CBlockItem]
    simplifyControlFlowBlocks ((CBlockStmt (CIf cond cr mCe _)):bs) rest = [CBlockStmt newIf]
      where
        newRest = (simplifyControlFlowBlocks bs rest) ++ rest
        sCFStmt :: CStat -> CStat
        sCFStmt r@(CReturn _ _) = r
        sCFStmt (CCompound lbls stmts _) = CCompound lbls (simplifyControlFlowBlocks stmts newRest) undefNode
        sCFStmt s = CCompound [] ((CBlockStmt s):newRest) undefNode
        newIf =
          case mCe of
            Just ce -> CIf cond (sCFStmt cr) (Just $ sCFStmt ce) undefNode
            Nothing -> CIf cond (sCFStmt cr) (Just $ CCompound [] newRest undefNode) undefNode
    simplifyControlFlowBlocks [] rest = rest
    simplifyControlFlowBlocks (s@(CBlockStmt (CReturn _ _)):[]) _ = [s]
    simplifyControlFlowBlocks ((CBlockStmt (CReturn _ _)):_:_) _ = error "Code found after a return!"
    simplifyControlFlowBlocks (b:bs) rest = b:(simplifyControlFlowBlocks bs rest)
