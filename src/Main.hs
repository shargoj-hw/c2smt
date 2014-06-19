{-# LANGUAGE DeriveDataTypeable #-}

import           System.Console.CmdArgs.Implicit

import           Language.C
import           SMTLib2

import           AstTransformers
import           Translator
import           Utils

-- For repl stuff
import           CExamples

data Config =
  Config
  {
    loopUnrolls :: Integer -- Number of loop unrollings to perform
  , fileName    :: String
  }
  deriving (Show, Data, Typeable)

defaultConfig :: Config
defaultConfig =
  Config { loopUnrolls = 10
                         &= help "Number of loop unrollings to performper loop"
         , fileName = def &= argPos 0
  }

main :: IO ()
main = do
  config <- cmdArgs defaultConfig
  let fn = fileName config
  cprog <- readFile fn
  let tu = parseFromString cprog
  print $ pretty tu
  putStrLn ""
  putStrLn "Translated:"
  print $ doTranslate config tu

doTranslate :: Config -> CTranslUnit -> String
doTranslate c tu = show . pp . translate $ runC2SMT c tu

runC2SMT :: Config -> CTranslUnit -> CTranslUnit
runC2SMT c =
  simplifyControlFlow .
  moveDeclsToTop .
  removeAssnOps .
  splitDeclsAndAssn .
  singleReturnify .
  unrollLoops (loopUnrolls c)
