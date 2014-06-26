{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import           Control.Applicative
import           Data.Generics
import           Language.C

parseFromString :: String -> CTranslUnit
parseFromString s =
  case parseC (inputStreamFromString s) nopos of
    Left err -> error $ show err
    Right tu -> tu

parseStmtFromString :: String -> CStat
parseStmtFromString s =
  case execParser statementP (inputStreamFromString s) nopos [] newNameSupply of
    Left err -> error $ show err
    Right (tu, _) -> tu

findFn :: CTranslUnit -> String -> Maybe CFunDef
findFn tu name = everything (<|>) (Nothing `mkQ` matchingFun) tu
  where
    matchingFun :: CFunDef -> Maybe CFunDef
    matchingFun c =
      if identToString (fnId c) == name then Just c else Nothing

fnId :: CFunDef -> Ident
fnId (CFunDef _ (CDeclr (Just ident) _ _ _ _) _ _ _) = ident

fnBody :: CFunDef -> CStat
fnBody (CFunDef _ _ _ body _) = body

everywhereM' :: Monad m => GenericM m -> GenericM m
everywhereM' f x = do
  x' <- f x
  gmapM (everywhereM' f) x'

declSpec :: CDeclaration t -> [CDeclarationSpecifier t]
declSpec (CDecl specs _ _) = specs

declId :: CDeclaration t -> Ident
declId (CDecl _ ((Just (CDeclr (Just ident) _ _ _ _), _, _):_) _) =  ident

typesMatch :: (Typeable a, Typeable b) => a -> b -> Bool
typesMatch a b = typeOf a == typeOf b

compoundWith :: CStat -> ([CBlockItem] -> [CBlockItem]) -> CStat
compoundWith (CCompound ids stmts _) t = CCompound ids (t stmts) undefNode
compoundWith s t = CCompound [] (t [CBlockStmt s]) undefNode
