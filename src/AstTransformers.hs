-- TODO: debug mode

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module AstTransformers where

import           Control.Applicative  ((<$>))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Data
import           Data.Foldable        (foldlM)
import           Data.Generics        hiding (empty)
import           Data.Graph
import           Data.List            (find, nub)
import           Data.Map             hiding (foldl, foldr)
import           Data.Maybe           (fromJust)
import           Language.C
import           Unsafe.Coerce

import           Config
import           Utils

-- For playing in the REPL
import           CExamples
import           Debug.Trace

-- This monad lets us implicitly pass around the config, keep track of
-- the gensym counter, etc.

type TransformerState = Integer

newtype TransformerM a =
  TM { unTM :: StateT TransformerState (Reader Config) a }
  deriving (Monad, MonadReader Config, MonadState TransformerState)

runTransformer :: TransformerM a -> Config -> a
runTransformer a = runReader (evalStateT (unTM a) initialState)
  where initialState = 1

-- TODO: How the heck do I do break/continue?
-- TODO: assert the not at the end of the code!!!
unrollLoops :: Data a => a -> TransformerM a
unrollLoops tu =
  do n <- asks loopUnrolls
     return $ unrollLoops n tu
  where
    unrollLoops :: Data a => Integer -> a -> a
    unrollLoops n = everywhere (mkT unrollLoops')
      where
        ni = undefNode
        unrollWhile :: CExpr -> CStat -> Integer -> CStat
        unrollWhile expr stmt 0 = CIf expr stmt Nothing ni
        unrollWhile expr stmt _ = CIf expr rest Nothing ni
          where
            rest =
              case stmt of
                CCompound is stmts _ ->
                  CCompound is
                  (stmts ++ [CBlockStmt $ unrollWhile expr stmt (n - 1)]) ni
                s ->
                  CCompound []
                  [CBlockStmt s, CBlockStmt $ unrollWhile expr stmt (n - 1)] ni
        unrollLoops' :: CStat -> CStat
        unrollLoops' (CWhile expr stmt isDoWhile _)
          | not isDoWhile = unrollWhile expr stmt (n - 1)
          | otherwise = CCompound [] [CBlockStmt stmt, CBlockStmt $ unrollWhile expr stmt (n - 1)]  ni
        unrollLoops' (CFor init until update stmt node) = undefined init until update stmt node
        unrollLoops' s = s

removeAssnOps :: Data a => a -> TransformerM a
removeAssnOps tu = return $ everywhere (mkT removeAssnOps') tu
  where
    removeAssnOps' :: CExpr -> CExpr
    removeAssnOps' (CAssign op exprL exprR node) =
      CAssign CAssignOp exprL exprR' node
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

moveDeclsToTop :: Data a => a -> TransformerM a
moveDeclsToTop a = return $ everywhere (mkT doMoveDecls) a
  where
    decls :: CFunDef -> [CDecl]
    decls = listify (\ (_ :: CDecl) -> True)
    removeDecls :: [CBlockItem] -> [CBlockItem]
    removeDecls (CBlockDecl _:xs) = xs
    removeDecls i = i
    doMoveDecls :: CFunDef -> CFunDef
    doMoveDecls fd =
      CFunDef declspecs declr decl (CCompound ns (ds++bis) sni) ni
      where
        ds = fmap CBlockDecl (decls fd)
        (CFunDef declspecs declr decl (CCompound ns bis sni) ni)
          = everywhere (mkT removeDecls) fd

splitDeclsAndAssn :: Data a => a -> TransformerM a
splitDeclsAndAssn a = return $ everywhere (mkT splitDeclsAndAssn') a
  where
    splitDeclsAndAssn' :: [CBlockItem] -> [CBlockItem]
    splitDeclsAndAssn' = concatMap splitDecl
    splitDecl (CBlockDecl (CDecl declspecs decls _)) = concatMap splitup decls
      where
        splitup (declr@(Just declr'), Just (CInitExpr initialier _), expr) =
          [CBlockDecl (CDecl declspecs [(declr, Nothing, expr)] undefNode),
           CBlockStmt (CExpr
                       (Just (CAssign CAssignOp (d2e declr') initialier undefNode))
                       undefNode)]
        -- initialization lists not supported (yet?):
        splitup (Just _, Just _, _) =
          error "Initialization lists not supported"
        splitup di = [CBlockDecl (CDecl declspecs [di] undefNode)]
    splitDecl bi = [bi]
    d2e (CDeclr (Just i) _ _ _ _) = CVar i undefNode

singleReturnify :: Data a => a -> TransformerM a
singleReturnify a = return $ everywhere (mkT returnifyFunction) a
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
        retValDecl = CDeclr (Just retId) [] Nothing [] undefNode
        declRetval = [CBlockDecl $ CDecl retType [(Just retValDecl, Nothing, Nothing)] undefNode]
        updatedBlocks = everywhere (mkT returnToRetval) body
        newReturn = [CBlockStmt $ CReturn (Just $ CVar retId undefNode) undefNode]
        returnToRetval :: CStat -> CStat
        returnToRetval (CReturn (Just v) _) =
          CExpr (Just $ CAssign CAssignOp (CVar retId undefNode) v undefNode) undefNode
        returnToRetval s = s

simplifyControlFlow :: Data a => a -> TransformerM a
simplifyControlFlow a = return $ everywhere (mkT simplifyControlFlowFunc) a
  where
    simplifyControlFlowFunc :: CFunDef -> CFunDef
    simplifyControlFlowFunc (CFunDef specs decl params (CCompound l b _) _) =
      CFunDef specs decl params (CCompound l (simplifyControlFlowBlocks b []) undefNode) undefNode
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

---- Function Inlining stuff

fundefsOrderedByCalls :: CTranslUnit -> [CFunDef]
fundefsOrderedByCalls tu = calls
  where
    (g, toN, _) = tuGraph tu
    --
    calls = fmap (fromJust . findFn tu . (\(_,b,_)->b) . toN)
            $ reverse
            $ topSort g
    --
    tuGraph tu = graphFromEdges $
                 (\(a,(b,c))->(a,b,c)) <$>
                 zip [1..] (nameToCalls tu)
    --
    nameToCalls tu = fmap
                     (\ fd->
                       (getFunName fd,
                        nub $ fmap getId (listify (isCall) fd)))
                     decls
    --
    isCall :: CExpr -> Bool
    isCall (CCall (CVar _ _) _ _) = True
    isCall _ = False
    --
    getId (CCall (CVar ident _) _ _) = identToString ident
    --
    getFunName :: CFunDef -> String
    getFunName (CFunDef _ (CDeclr (Just ident) _ _ _ _) _ _ _) =
      identToString ident
    --
    decls = listify (\ (_ :: CFunDef) -> True) tu

seperateFnCalls :: CStat -> TransformerM CStat
seperateFnCalls (CCompound ids stmts _) = do
  newcalls <- mapM seperateFnCallsBlockM stmts
  return $ CCompound ids (concat newcalls) undefNode
  where
    seperateFnCallsExprM :: CExpr -> State (Integer, [(Ident, CExpr)]) CExpr
    seperateFnCallsExprM c@(CCall (CVar ident _) _ _) = do
      (i, id2funcall) <- get
      let newId = internalIdent $ identToString ident ++ "_call_" ++ show i
      put (i + 1, id2funcall ++ [(newId, c)])
      return $ CVar newId undefNode
    seperateFnCallsExprM e = return e
    --
    seperateFnCallsBlockM :: CBlockItem -> TransformerM [CBlockItem]
    seperateFnCallsBlockM (CBlockStmt s) = do
      n <- get
      let (s', (n', calls)) =
            runState (everywhereM (mkM seperateFnCallsExprM) s) (n, [])
      put n'
      return $ fmap declare calls ++  [CBlockStmt s']
        where declare (ident, expr) =
                CBlockDecl
                $ CDecl [CTypeSpec $ CFloatType undefNode]
                [(Just $ CDeclr (Just ident) [] Nothing [] undefNode,
                  Just $ CInitExpr expr undefNode,
                  Nothing)] undefNode
    seperateFnCallsBlockM i = return [i]
seperateFnCalls s = return s

type UNState a = State (Integer, Env) a
type Env = Map Ident Ident

uniqueNameify :: Data a => a -> State (Integer, Env) a
uniqueNameify = everywhereM' uniqueNameify'
  where
    uniqueNameify' :: forall a. Typeable a => a -> UNState a
    uniqueNameify' a
      | typesMatch a (undefined :: CExpression NodeInfo) =
        unsafeCoerce $ uniqueNamesE (unsafeCoerce a)
      | typesMatch a (undefined :: CDeclarator NodeInfo) =
          unsafeCoerce $ uniqueNamesD (unsafeCoerce a)
      | otherwise = return a
    uniqueNamesD :: CDeclr -> UNState CDeclr
    uniqueNamesD (CDeclr (Just ident) dclr lit attrs ni) = do
      (i, idMap) <- get
      let newId = gensym i ident
      put (i + 1, insert ident newId idMap)
      return $ CDeclr (Just newId) dclr lit attrs ni
    --
    uniqueNamesE :: CExpr -> UNState CExpr
    uniqueNamesE (CVar ident ni) = do
      (_, idMap) <- get
      return $ CVar (idMap ! ident) ni
    uniqueNamesE e = return e

gensym :: Show a => a -> Ident -> Ident
gensym i n =
  let nameStr = identToString n
  in internalIdent $ "GENSYM_" ++ nameStr ++ "_" ++  show i

linearizeFunCalls :: Data a => a -> [CFunDef] -> TransformerM a
linearizeFunCalls funDef defs = everywhereM (mkM linearize) funDef
  where
    idToFunDef :: Ident -> TransformerM CFunDef
    idToFunDef ident = do
      let name = identToString ident
          -- This will have to change when we support stuff like sqrt
          Just fn = find (\f -> identToString (fnId f) == name) defs
      i  <- get
      let (fn', (i', _)) = runState (uniqueNameify fn) (i, empty)
      put i'
      return fn'
    --
    linearize :: CStat -> TransformerM CStat
    linearize (CCompound ids stmts _) = do
      stmts' <- mapM linearizeStmt stmts
      return $ CCompound ids (concat stmts') undefNode
    linearize s = return s
    --
    linearizeStmt :: CBlockItem -> TransformerM [CBlockItem]
    linearizeStmt cb@(CBlockDecl d@(CDecl _ decls _))
      | linearizble decls = do
        let linearizeDecl (Just declr, Just (CInitExpr expr _), _) = do
              let CCall (CVar ident _) params _ = expr
              CFunDef fdeclspecs fdecl _ body _ <- idToFunDef ident
              let argdecls = listify (\ (_ :: CDecl) -> True) fdecl
                  argItems = paramItems argdecls params
              return $
                [CBlockDecl $
                 CDecl fdeclspecs [(Just declr, Nothing, Nothing)] undefNode]
                ++ fmap CBlockDecl argdecls
                ++ argItems
                ++ [CBlockStmt (replaceReturn (declId d) body)]
        bis <- mapM linearizeDecl decls
        return $ concat bis
      | otherwise = return [cb]
    linearizeStmt i = return [i]
    --
    linearizble = all (\case {(Just _, Just _, _) -> True; _ -> False})
    --
    paramItems decls params = fmap doParamItem (zip decls params)
      where
        doParamItem (decl, param) =
          let ident = declId decl
              var = CVar ident undefNode
              assign = CAssign CAssignOp var param undefNode
          in CBlockStmt $ CExpr (Just assign) undefNode
    --
    replaceReturn varid = everywhere (mkT replace)
      where
        replace (CReturn (Just expr) _) =
          CExpr (Just
                 $ CAssign CAssignOp (CVar varid undefNode) expr undefNode)
          undefNode
        replace x = x

linearize :: CTranslUnit -> TransformerM CTranslUnit
linearize tu = do tu' <- splitDeclsAndAssn tu
                  tu'' <- everywhereM (mkM seperateFnCalls) tu'
                  let fundefs = fundefsOrderedByCalls tu''
                  fundefs' <- foldlM linearizeIt [] fundefs
                  return $
                    CTranslUnit (reverse $ fmap CFDefExt fundefs') undefNode
  where
    linearizeIt :: [CFunDef] -> CFunDef -> TransformerM [CFunDef]
    linearizeIt fns fn = do
      fn' <- linearizeFunCalls fn fns
      return $ fn':fns

-- doit =
--   let Just main = findFn (splitDeclsAndAssn example1) "main"
--       defs = fundefsOrderedByCalls (splitDeclsAndAssn example1)
--       main2 = evalState (everywhereM (mkM seperateFnCalls) main) 1
--   in linearizeFunCalls main2 defs
