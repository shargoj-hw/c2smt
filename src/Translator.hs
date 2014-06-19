{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Translator (translate) where

import           Data.Binary.IEEE754
import           Data.Bits
import           Data.Generics
import           Data.Maybe          (catMaybes, fromJust)
import           Language.C
import           SMTLib2
import           Text.PrettyPrint

import           AstTransformers
import           CExamples
import           Utils

floatingPoint :: SMTLib2.Ident
floatingPoint = I (N "FloatingPoint") [8, 24]

-- TODO: make sure this is right in terms of endianness, etc
splitFP :: Float -> (Integer, Integer, Integer)
splitFP f = (nanbit, integerize top 0, integerize bottom 0)
  where
    bits = map (\b -> if testBit (floatToWord f) b then 1 else 0) [0..32]
    (nanbit, top, bottom) = (bits !! 8, take 8 bits, drop 9 bits)
    integerize bs n = foldl (\num b-> shiftL (b .|. num) 1) n bs

fp2bv :: Float -> Expr
fp2bv f = App (I (N "fp") []) Nothing [Lit nanbit', Lit top', Lit bottom']
  where
    (nanbit, top, bottom) = splitFP f
    (nanbit', top', bottom') = (LitBV nanbit 1, LitBV top 8, LitBV bottom 23)

floatDecl :: String -> Command
floatDecl name = CmdDeclareFun (N name) [] (TApp floatingPoint [])

eAnd :: [Expr] -> Expr
eAnd [] = eEmpty
eAnd es = App (I (N "and") []) Nothing es

eOr :: [Expr] -> Expr
eOr [] = eEmpty
eOr es = App (I (N "or") []) Nothing es

eName :: String -> Expr
eName name = App (I (N name) []) Nothing []

eEmpty :: Expr
eEmpty = App (I (N "") []) Nothing []

translate :: CTranslationUnit NodeInfo -> Script
translate tu = Script cmds
  where
    CCompound _ block_items _ = fnBody . fromJust $ findFn tu "main"
    cmds = (CmdSetLogic $ N "QF_FP") : concatMap block2cmds block_items

block2cmds :: CBlockItem -> [Command]
block2cmds (CBlockStmt stmt) = [stmt2cmd stmt]
block2cmds (CBlockDecl decl) = fmap floatDecl ids
  where
    CDecl _ decls _ = decl
    ids = fmap identToString $
          catMaybes $ fmap (\(CDeclr ident _ _ _ _)->ident) $
          catMaybes $ fmap (\(declr, _, _) -> declr) decls

block2exprs :: [CBlockItem] -> [Expr]
block2exprs =
  fmap stmt2expr . catMaybes . fmap (\i -> case i of CBlockStmt s -> Just s; _ -> Nothing)

stmt2cmd :: CStat -> Command
stmt2cmd (CReturn _ _) = CmdExit
stmt2cmd stmt = CmdAssert . stmt2expr $ stmt

stmt2expr :: CStat -> Expr
stmt2expr (CCompound _ block_items _) = eAnd $ block2exprs block_items
stmt2expr (CExpr (Just e) _) = expr2expr e
stmt2expr (CIf condExpr thenStmt (Just elseStmt) _) =
  eOr [eAnd [condExpr', thenExpr], eAnd [condExpr', elseExpr]]
  where
    condExpr' = expr2expr condExpr
    thenExpr = stmt2expr thenStmt
    elseExpr = stmt2expr elseStmt
-- TODO: remove this because it should never happen after simplification.
stmt2expr (CIf condExpr thenStmt Nothing _) =
  eOr [eAnd [condExpr', thenExpr]]
  where
    condExpr' = expr2expr condExpr
    thenExpr = stmt2expr thenStmt
stmt2expr (CExpr Nothing _) = undefined
stmt2expr s = error $ "Unsupported statement passed to translator: " ++ show s

-- Convert ints into float
expr2expr :: CExpr -> Expr
expr2expr (CAssign _ left right _) =
  App (I (N "fp.eq") []) Nothing [expr2expr left, expr2expr right]
expr2expr (CBinary op left right _) =
  App (I (N fpOp) []) Nothing [expr2expr left, expr2expr right]
  where
    fpOp =
      case op of
        CMulOp -> "fp.mul"
        CDivOp -> "fp.div"
        CRmdOp -> "fp.rem"
        CAddOp -> "fp.add"
        CSubOp -> "fp.sub"
        CLeOp -> "fp.lt"
        CGrOp -> "fp.gt"
        CLeqOp -> "fp.lte"
        CGeqOp -> "fp.gre"
        CEqOp -> "fp.eq"
        CNeqOp -> "fp.neq"
        CAndOp -> "fp.and"
        CXorOp -> "fp.xor"
        COrOp -> "fp.or"
        CLndOp -> "fp.and"
        CLorOp -> "fp.or"
expr2expr (CVar ident _) = eName . identToString $ ident
expr2expr (CConst (CFloatConst (CFloat sf) _)) = fp2bv . read $  sf
expr2expr (CConst (CIntConst (CInteger n _ _) _)) = Lit $ LitNum n
-- TODO: we're gonna probably want to support this stuff
-- expr2expr (CIndex (CExpression a) (CExpression a) _) = undefined
-- expr2expr (CCall (CExpression a) [CExpression a] _) = undefined
-- expr2expr (CMember (CExpression a) Ident Bool _) = undefined
-- expr2expr (CStatExpr (CStatement a) _) = undefined
-- expr2expr (CUnary CUnaryOp (CExpression a) _) = undefined
expr2expr e = error $ "Unsupported expression passed to translator: " ++ show e
