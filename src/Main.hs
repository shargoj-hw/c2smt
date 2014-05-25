{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           System.Console.GetOpt
import           System.Environment    (getArgs)

import           Language.C
import           SMTLib2

import           AstTransformers
import           Translator

-- For repl stuff
import           CExamples

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = getOpt RequireOrder options args
  print $ show nonOpts

options =
  [
--    Option ['r'] ["numUnroll"] (OptArg )
  ]

runC2SMT :: CTranslUnit -> CTranslUnit
runC2SMT =
  simplifyControlFlow .
  moveDeclsToTop .
  removeAssnOps .
  splitDeclsAndAssn .
  singleReturnify .
  unrollLoops 5
