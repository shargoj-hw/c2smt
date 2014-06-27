import           System.Console.CmdArgs.Implicit

import           Language.C
import           SMTLib2

import           AstTransformers
import           Config
import           Translator
import           Utils

-- For repl stuff
import           CExamples

main :: IO ()
main = do
  config <- cmdArgs defaultConfig
  let fn = fileName config
  cprog <- readFile fn
  let tu = parseFromString cprog
  putStrLn "Original:"
  print $ pretty tu
  putStrLn ""
  putStrLn "Translated:"
  print . pp . translate . runTransformer config . transform $ tu
