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

-- * Translation Functions

-- | Translates a [limited] C AST to an SMTLib AST. The output is the
-- body of the main function.
translate :: CTranslUnit -> Script
translate tu = Script cmds
  where
    CCompound _ block_items _ = fnBody . fromJust $ findFn tu "main"
    cmds = (CmdSetLogic $ N "QF_FP") : concatMap translateTopLevel block_items

-- | Translates statements and declarations in the body of the "main"
-- function.
translateTopLevel :: CBlockItem -> [Command]
translateTopLevel (CBlockStmt stmt) = [translateTopLevelStmt stmt]
translateTopLevel (CBlockDecl decl) = fmap floatDecl ids
  where
    CDecl _ decls _ = decl
    ids = fmap identToString $
          catMaybes $ fmap (\(CDeclr ident _ _ _ _)->ident) $
          catMaybes $ fmap (\(declr, _, _) -> declr) decls

-- | Translates a statement and asserts it.
translateTopLevelStmt :: CStat -> Command
translateTopLevelStmt (CReturn _ _) = CmdExit
translateTopLevelStmt stmt = CmdAssert . translateStmt $ stmt

-- | Translates each of the statements in the list of block items to
-- expressions.
translateBlockItems :: [CBlockItem] -> [Expr]
translateBlockItems = fmap translateStmt . catMaybes . fmap onlyStmts
  where onlyStmts i = case i of CBlockStmt s -> Just s; _ -> Nothing

-- | Translates a C Statement AST to an SMTLib expression.
translateStmt :: CStat -> Expr
translateStmt (CCompound _ block_items _) = eAnd $ translateBlockItems block_items
translateStmt (CExpr (Just e) _) = translateExpr e
translateStmt (CIf condExpr thenStmt (Just elseStmt) _) =
  eOr [eAnd [condExpr', thenExpr], eAnd [eNot condExpr', elseExpr]]
  where
    condExpr' = translateExpr condExpr
    thenExpr = translateStmt thenStmt
    elseExpr = translateStmt elseStmt
translateStmt (CIf condExpr thenStmt Nothing _) =
  eOr [eAnd [condExpr', thenExpr]]
  where
    condExpr' = translateExpr condExpr
    thenExpr = translateStmt thenStmt
translateStmt (CExpr Nothing _) = undefined
translateStmt s = transError "Unsupported statement passed to translator:" s

-- | Translates a C expression to an SMTLib expression
translateExpr :: CExpr -> Expr
translateExpr (CAssign _ left right _) =
  App (I (N "fp.eq") []) Nothing [translateExpr left, translateExpr right]
translateExpr (CBinary op left right _) =
  App (I (N fpOp) []) Nothing [translateExpr left, translateExpr right]
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
translateExpr (CVar ident _) = eName . identToString $ ident
translateExpr (CConst (CFloatConst (CFloat sf) _)) = fp2bv . read $  sf
translateExpr (CConst (CIntConst (CInteger n _ _) _)) = fp2bv . fromIntegral $ n
-- TODO: we're gonna probably want to support this stuff
-- translateExpr (CIndex (CExpression a) (CExpression a) _) = undefined
-- translateExpr (CCall (CExpression a) [CExpression a] _) = undefined
-- translateExpr (CMember (CExpression a) Ident Bool _) = undefined
-- translateExpr (CStatExpr (CStatement a) _) = undefined
-- translateExpr (CUnary CUnaryOp (CExpression a) _) = undefined
translateExpr e = transError "Unsupported expression passed to translator: " e

-- * Translation Helper Functions

-- | SMTLib ident for FloatingPoint declarations
floatingPoint :: SMTLib2.Ident
floatingPoint = I (N "FloatingPoint") [8, 24]

-- TODO: make sure this is right in terms of endianness, etc
-- | Translates some Haskell float to the SMTLib floating point spec.
fp2bv :: Float -> Expr
fp2bv f = App (I (N "fp") []) Nothing [Lit nanbit', Lit top', Lit bottom']
  where
    (nanbit, top, bottom) = splitFP f
    (nanbit', top', bottom') = (LitBV nanbit 1, LitBV top 8, LitBV bottom 23)
    --
    splitFP :: Float -> (Integer, Integer, Integer)
    splitFP f_ = (nanbit_, integerize top_ 0, integerize bottom_ 0)
      where
        bits = reverse $ map (\b -> if testBit (floatToWord f_) b then 1 else 0) [0..32]
        (nanbit_, top_, bottom_) = (head bits, take 8 (drop 1 bits), drop 9 bits)
        integerize bs n = foldl (\num b-> shiftL (b .|. num) 1) n bs

-- | Declare some floating point value.
floatDecl :: String -> Command
floatDecl name = CmdDeclareFun (N name) [] (TApp floatingPoint [])

-- | 'ands' the list of expressions.
eAnd :: [Expr] -> Expr
eAnd [] = eEmpty
eAnd es = App (I (N "and") []) Nothing es

-- | 'ors' the list of expressions.
eOr :: [Expr] -> Expr
eOr [] = eEmpty
eOr es = App (I (N "or") []) Nothing es

-- | 'nots' the expression.
eNot :: Expr -> Expr
eNot e = App (I (N "not") [] ) Nothing [e]

-- | Returns this string as the name
eName :: String -> Expr
eName name = App (I (N name) []) Nothing []

-- | Blank expressions
eEmpty :: Expr
eEmpty = App (I (N "") []) Nothing []

-- | Errors out, printing the pretty and raw value of the AST that
-- caused the error.
transError :: (Show a, Pretty a) => String -> a -> t
transError m c = error $ m ++ "\nC value:\n" ++ show (pretty c) ++ "\nAST value:" ++ show c
