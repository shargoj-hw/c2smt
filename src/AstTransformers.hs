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
import           Control.Arrow        (first, second)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Data
import           Data.Foldable        (foldlM)
import           Data.Generics        hiding (empty)
import           Data.Graph
import           Data.List            (find, nub)
import           Data.Map             hiding (foldl, foldr)
import           Data.Maybe           (catMaybes, fromJust, fromMaybe,
                                       maybeToList)
import           Debug.Trace
import           Language.C
import           Unsafe.Coerce

import           Config
import           Utils

-- For playing in the REPL
import           CExamples

-- | Encapsulates all of the mutable state required by 'TransformerM'
type TransformerState = Integer

-- | This monad lets us implicitly pass around the config, keep track of
-- the gensym counter, etc.
newtype TransformerM a =
  TM { unTM :: StateT TransformerState (Reader Config) a }
  deriving (Monad, MonadReader Config, MonadState TransformerState)

-- | Runs the transformation using the given config.
runTransformer ::  Config -> TransformerM a  -> a
runTransformer c a = runReader (evalStateT (unTM a) initialState) c
  where initialState = 1

-- | Runs the transformation using the default config
runTransformer_ :: TransformerM a -> a
runTransformer_ = runTransformer defaultConfig

-- | List of all of the transformation functions in the order they
-- should be applied, along with their name-strings for debugging.
transformerSteps :: [(CTranslUnit -> TransformerM CTranslUnit, String)]
transformerSteps =
  [
    (unrollLoops, "unrollLoops"),
    (simplifyControlFlow, "simplifyControlFlow"),
    (ssaAll, "ssaAll"),
    (singleReturnify, "singleReturnify"),
    (removeAssnOps, "removeAssnOps"),
    (splitDeclsAndAssn, "splitDeclsAndAssn"),
    (inlineFunctions, "inlineFunctions"),
    (moveDeclsToTop, "moveDeclsToTop")
  ]

-- | Combines everything from 'transformerSteps', tracing each step if
-- in debug mode.
transform :: CTranslUnit -> TransformerM CTranslUnit
transform tu = foldM app tu transformerSteps
  where app v (t, s) =
          do debug <- asks debugMode
             v' <- t v
             let deb = "---> "++ s ++ ": \n" ++ show (pretty v')
             return $ if debug then trace deb v' else v'

-- TODO: How the heck do I do break/continue?
-- TODO: assert the not at the end of the code!!!
-- | Unrolls each loop 'loopUnrolls' number of times.
unrollLoops :: Data a => a -> TransformerM a
unrollLoops tu =
  do n <- asks loopUnrolls
     return $ doUnrollLoops n tu
  where
    doUnrollLoops :: Data a => Integer -> a -> a
    doUnrollLoops n = everywhere (mkT unrollLoops')
      where
        unrollLoops' (CWhile expr stmt isDoWhile _)
          | not isDoWhile = unrollWhile expr stmt (n - 1)
          | otherwise = CCompound [] [CBlockStmt stmt,
                                      CBlockStmt
                                      $ unrollWhile expr stmt (n - 1)]  un
        unrollLoops' (CFor init until update stmt _) =
          CCompound [] [initItem, CBlockStmt $ unrollFor cond update stmt (n - 1)] un
          where
            initItem =
              case init of
                Left e -> CBlockStmt $ CExpr e undefNode
                Right decl  -> CBlockDecl decl
            cond = fromMaybe (CVar (internalIdent "true") un) until
        unrollLoops' s = s
        --
        unrollWhile :: CExpr -> CStat -> Integer -> CStat
        unrollWhile expr stmt 0 = CIf expr stmt Nothing un
        unrollWhile expr stmt n' = CIf expr rest Nothing un
          where
            rest =
              case stmt of
                CCompound is stmts _ ->
                  CCompound is
                  (stmts ++ [CBlockStmt $ unrollWhile expr stmt (n' - 1)]) un
                s ->
                  CCompound []
                  [CBlockStmt s, CBlockStmt $ unrollWhile expr stmt (n' - 1)] un
        --
        unrollFor :: CExpr -> Maybe CExpr -> CStat -> Integer -> CStat
        unrollFor cond update body n = CIf cond body' Nothing un
          where
            body' =
              (compoundWith body (\s -> update' ++ s ++ rest))
            update' =
              fmap (\s -> CBlockStmt . flip CExpr un . Just $ s) (maybeToList update)
            rest =
              if n == 0 then [] else [CBlockStmt $ unrollFor cond update body (n-1)]


-- | Replaces e.g., a += b with a = a + b.
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
            CMulAssOp -> CBinary CMulOp exprL exprR un
            CDivAssOp -> CBinary CDivOp exprL exprR un
            CRmdAssOp -> CBinary CRmdOp exprL exprR un
            CAddAssOp -> CBinary CAddOp exprL exprR un
            CSubAssOp -> CBinary CSubOp exprL exprR un
            CShlAssOp -> CBinary CShlOp exprL exprR un
            CShrAssOp -> CBinary CShrOp exprL exprR un
            CAndAssOp -> CBinary CAndOp exprL exprR un
            CXorAssOp -> CBinary CXorOp exprL exprR un
            COrAssOp -> CBinary COrOp exprL exprR un
    removeAssnOps' e = e

-- | Takes every declaration and moves it to the top.
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

-- | Replaces e.g., float a = 3 with float a; a = 3.
splitDeclsAndAssn :: Data a => a -> TransformerM a
splitDeclsAndAssn a = return $ everywhere (mkT splitDeclsAndAssn') a
  where
    splitDeclsAndAssn' :: [CBlockItem] -> [CBlockItem]
    splitDeclsAndAssn' = concatMap splitDecl
    splitDecl (CBlockDecl (CDecl declspecs decls _)) = concatMap splitup decls
      where
        splitup (declr@(Just declr'), Just (CInitExpr initialier _), expr) =
          [CBlockDecl (CDecl declspecs [(declr, Nothing, expr)] un),
           CBlockStmt (CExpr
                       (Just (CAssign CAssignOp (d2e declr') initialier un))
                       un)]
        -- initialization lists not supported (yet?):
        splitup (Just _, Just _, _) =
          error "Initialization lists not supported"
        splitup di = [CBlockDecl (CDecl declspecs [di] un)]
    splitDecl bi = [bi]
    d2e (CDeclr (Just i) _ _ _ _) = CVar i un

-- | Transforms functions to only return once, at the end.
singleReturnify :: Data a => a -> TransformerM a
singleReturnify a = return $ everywhere (mkT returnifyFunction) a
  where
    returnifyFunction :: CFunDef -> CFunDef
    returnifyFunction (CFunDef specs decl params body _) =
      let CCompound localLabels blockItems _ = body
          body' = CCompound localLabels (returnifyBody specs blockItems) un
      in CFunDef specs decl params body' un
    returnifyBody :: [CDeclSpec] -> [CBlockItem] -> [CBlockItem]
    returnifyBody retType body = declRetval ++ updatedBlocks ++ newReturn
      where
        retId = internalIdent "GENERATEDRETVAL"
        retValDecl = CDeclr (Just retId) [] Nothing [] un
        declRetval = [CBlockDecl $ CDecl retType [(Just retValDecl, Nothing, Nothing)] un]
        updatedBlocks = everywhere (mkT returnToRetval) body
        newReturn = [CBlockStmt $ CReturn (Just $ CVar retId un) un]
        returnToRetval :: CStat -> CStat
        returnToRetval (CReturn (Just v) _) =
          CExpr (Just $ CAssign CAssignOp (CVar retId un) v un) un
        returnToRetval s = s

-- | Reorders a C program so that there are no statements after
-- conditionals.
simplifyControlFlow :: Data a => a -> TransformerM a
simplifyControlFlow a = return $ everywhere (mkT simplifyControlFlowFunc) a
  where
    simplifyControlFlowFunc :: CFunDef -> CFunDef
    simplifyControlFlowFunc (CFunDef specs decl params (CCompound l b _) _) =
      CFunDef specs decl params (CCompound l (simplifyControlFlowBlocks b []) un) un
    simplifyControlFlowBlocks :: [CBlockItem] -> [CBlockItem] -> [CBlockItem]
    simplifyControlFlowBlocks (CBlockStmt (CIf cond cr mCe _):bs) rest = [CBlockStmt newIf]
      where
        newRest = simplifyControlFlowBlocks bs rest ++ rest
        sCFStmt :: CStat -> CStat
        sCFStmt r@(CReturn _ _) = r
        sCFStmt (CCompound lbls stmts _) = CCompound lbls (simplifyControlFlowBlocks stmts newRest) un
        sCFStmt s = CCompound [] (CBlockStmt s:newRest) un
        newIf =
          case mCe of
            Just ce -> CIf cond (sCFStmt cr) (Just $ sCFStmt ce) un
            Nothing -> CIf cond (sCFStmt cr) (Just $ CCompound [] newRest un) un
    simplifyControlFlowBlocks [] rest = rest
    simplifyControlFlowBlocks (s@(CBlockStmt (CReturn _ _)):[]) _ = [s]
    simplifyControlFlowBlocks (CBlockStmt (CReturn _ _):_:_) _ = error "Code found after a return!"
    simplifyControlFlowBlocks (b:bs) rest = b:simplifyControlFlowBlocks bs rest

---- Function Inlining stuff

-- | Returns a list of the function definitions in order of the least
-- calls to other functions to the most.
fundefsOrderedByCalls :: CTranslUnit -> [CFunDef]
fundefsOrderedByCalls tu = calls
  where
    calls = fmap (fromJust . findFn tu . (\(_,b,_)->b) . toN)
            $ reverse
            $ topSort g
    --
    (g, toN :: Vertex -> (Integer, String, [String]), _) = tuGraph
    --
    tuGraph =
      graphFromEdges $ (\(a,(b,c))->(a,b,c)) <$> zip [1..] nameToCalls
    --
    nameToCalls = fmap
                  (\ fd->
                    (getFunName fd,
                     nub $ fmap getId (listify isCall fd)))
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

-- | Rewrites expressions, declaring new variables for each function
-- call in the expression and replacing the calls with them.
--
-- NOTE: The new declarations are of the form float foo_call_3 =
-- foo(4). This form is required by inlineFunctionCalls.
seperateFnCalls :: CStat -> TransformerM CStat
seperateFnCalls (CCompound ids stmts _) = do
  newcalls <- mapM seperateFnCallsBlockM stmts
  return $ CCompound ids (concat newcalls) un
  where
    seperateFnCallsExprM :: CExpr -> State (Integer, [(Ident, CExpr)]) CExpr
    seperateFnCallsExprM c@(CCall (CVar ident _) _ _) = do
      (i, id2funcall) <- get
      let newId = internalIdent $ identToString ident ++ "_call_" ++ show i
      put (i + 1, id2funcall ++ [(newId, c)])
      return $ CVar newId un
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
                $ CDecl [CTypeSpec $ CFloatType un]
                [(Just $ CDeclr (Just ident) [] Nothing [] un,
                  Just $ CInitExpr expr un,
                  Nothing)] un
    seperateFnCallsBlockM i = return [i]
seperateFnCalls s = return s

type Env = Map Ident Ident

-- TODO: deal with stuff like "true"!
-- | Gensyms new names for each variable in the program.
uniqueNameify :: Data a => a -> TransformerM a
uniqueNameify a = do
  i <- get
  modify (+1)
  let rename (CVar ident _) = CVar (internalIdent $
                                    "un" ++
                                    show i ++
                                    "_" ++
                                    (identToString ident)) un
      rename e = e
  return $ everywhere (mkT rename) a

-- | Creates a new variable name.
gensym :: Show a => a -> Ident -> Ident
gensym i n =
  let nameStr = identToString n
  in internalIdent $ "GENSYM_" ++ nameStr ++ "_" ++  show i

-- | Replaces all calls (after seperateFnCalls has been run on the
-- data) with the functions found in defs.
inlineFunCalls :: Data a => a -> [CFunDef] -> TransformerM a
inlineFunCalls funDef defs = everywhereM (mkM inline) funDef
  where
    idToFunDef :: Ident -> TransformerM CFunDef
    idToFunDef ident = do
      let name = identToString ident
          -- This will have to change when we support stuff like sqrt
          Just fn = find (\f -> identToString (fnId f) == name) defs
      uniqueNameify fn
    --
    inline :: CStat -> TransformerM CStat
    inline (CCompound ids stmts _) = do
      stmts' <- mapM inlineStmt stmts
      return $ CCompound ids (concat stmts') un
    inline s = return s
    --
    inlineStmt :: CBlockItem -> TransformerM [CBlockItem]
    inlineStmt cb@(CBlockDecl d@(CDecl _ decls _))
      | inlinable decls = do
        let inlineDecl (Just declr, Just (CInitExpr expr _), _) = do
              let CCall (CVar ident _) params _ = expr
              CFunDef fdeclspecs fdecl _ body _ <- idToFunDef ident
              let argdecls = listify (\ (_ :: CDecl) -> True) fdecl
                  argItems = paramItems argdecls params
              return $
                [CBlockDecl $
                 CDecl fdeclspecs [(Just declr, Nothing, Nothing)] un]
                ++ fmap CBlockDecl argdecls
                ++ argItems
                ++ [CBlockStmt (replaceReturn (declId d) body)]
        bis <- mapM inlineDecl decls
        return $ concat bis
      | otherwise = return [cb]
    inlineStmt i = return [i]
    --
    inlinable = all (\case {(Just _, Just _, _) -> True; _ -> False})
    --
    paramItems decls params = fmap doParamItem (zip decls params)
      where
        doParamItem (decl, param) =
          let ident = declId decl
              var = CVar ident un
              assign = CAssign CAssignOp var param un
          in CBlockStmt $ CExpr (Just assign) un
    --
    replaceReturn varid = everywhere (mkT replace)
      where
        replace (CReturn (Just expr) _) =
          CExpr (Just
                 $ CAssign CAssignOp (CVar varid un) expr un)
          un
        replace x = x

-- | Linearizes all function calls.
inlineFunctions :: CTranslUnit -> TransformerM CTranslUnit
inlineFunctions tu = do
  tu' <- splitDeclsAndAssn tu
  tu'' <- everywhereM (mkM seperateFnCalls) tu'
  let fundefs = fundefsOrderedByCalls tu''
  fundefs' <- foldlM linearizeIt [] fundefs
  -- This is lazy on my part and removes global
  -- variables. When those go in, this has to change.
  return $
    CTranslUnit (reverse $ fmap CFDefExt fundefs') un
  where
    linearizeIt :: [CFunDef] -> CFunDef -> TransformerM [CFunDef]
    linearizeIt fns fn = do
      fn' <- inlineFunCalls fn fns
      return $ fn':fns

type SSAM = State (Integer, Env, [Ident])

ssa :: CFunDef -> TransformerM CFunDef
ssa (CFunDef ds d drs s _) = do
  i <- get
  let (s', (i', _, ids)) = runState (ssaStmt s) (i, initialMap, [])
      -- TODO: When you add ints, this needs to keep track of the types.
      declrs = fmap (\ident -> CBlockDecl
                               $ CDecl [CTypeSpec $ CFloatType un]
                               [(Just $ CDeclr (Just ident) [] Nothing [] un,
                                 Nothing, Nothing)] un
                    ) ids
  put i'
  return $ CFunDef ds d drs (compoundWith s' (declrs ++)) un
  where
    initialMap =
      let ids = catMaybes $ listify (\ (_:: Maybe Ident) -> True) drs
      in foldr (\ i m -> insert i i m) empty ids
    --
    addIdInit ident = modify $ \ (i,e,l) -> (i, insert ident ident e, l)
    --
    addId idFrom idTo = modify $ \ (i,e,l) -> (i, insert idFrom idTo e, idTo:l)
    --
    inc = modify $ \ (i,e,l) -> (i+1, e, l)
    --
    newIdent ident = do
      (i, m, e) <- get
      let ident' = internalIdent $ (identToString ident) ++ "__" ++ show i
      addId ident ident'
      inc
      return ident'
    --
    ssaBlocks :: CBlockItem -> SSAM CBlockItem
    ssaBlocks bd@(CBlockDecl decl) = do
      let did = declId decl
      addIdInit did
      return bd
    ssaBlocks bs@(CBlockStmt s) = do s' <- ssaStmt s; return $ CBlockStmt s'
    --
    ssaStmt :: CStat -> SSAM CStat
    ssaStmt (CIf cond thn (Just els) _) = do
      cond' <- ssaExpr cond
      initialState <- get
      thn' <- ssaStmt thn
      put initialState
      els' <- ssaStmt els
      return (CIf cond' thn' (Just els') un)
    ssaStmt (CIf cond thn Nothing _) = do
      cond' <- ssaExpr cond
      initialState <- get
      thn' <- ssaStmt thn
      put initialState
      return (CIf cond' thn' Nothing un)
    ssaStmt (CCompound ids stmts _) = do
      stmts' <- mapM ssaBlocks stmts
      return $ CCompound ids stmts' un
    ssaStmt (CExpr (Just e) _) = do
      e' <- ssaExpr e
      return $ CExpr (Just e') un
    ssaStmt (CReturn (Just e) _) = do
      e' <- ssaExpr e
      return $ CReturn (Just e') un
    ssaStmt s = return s
    --
    ssaExpr :: CExpr -> SSAM CExpr
    ssaExpr (CAssign op (CVar ident _) r _) = do
      -- This is a littttttle clunky, but it gets the job done!
      r' <- everywhereM (mkM ssaExpr) r
      ident' <- newIdent ident
      return $ CAssign op (CVar ident' un) r' un
    ssaExpr (CVar ident _) = do
      (_, m, l) <- get
      let ident' = fromMaybe ident (Data.Map.lookup ident m)
      return $ CVar ident' un
    ssaExpr e = return e

-- | Performs SSA everywhere.
ssaAll :: CTranslUnit -> TransformerM CTranslUnit
ssaAll = everywhereM' (mkM ssa)

-- | Saves on typing.
un :: NodeInfo
un = undefNode
