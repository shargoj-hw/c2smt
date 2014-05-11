{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

import System.Environment (getArgs)
import System.Console.GetOpt
import Language.Sexp.Parser
import Language.Sexp.Printer

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = getOpt RequireOrder options args
  putStrLn $ show nonOpts

options =
  [
--    Option ['r'] ["numUnroll"] (OptArg )
  ]
