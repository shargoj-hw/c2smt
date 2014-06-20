{-# LANGUAGE DeriveDataTypeable #-}

module Config (Config, defaultConfig, loopUnrolls, fileName, debugMode) where

import           Data.Data
import           System.Console.CmdArgs.Implicit

data Config =
  Config
  {
    loopUnrolls :: Integer
  , fileName    :: String
  , debugMode   :: Bool
  }
  deriving (Show, Data, Typeable)

defaultConfig :: Config
defaultConfig =
  Config { loopUnrolls = 10 &= help loopUnrollHelp
         , debugMode = False &= help debugModeHelp
         , fileName = def &= argPos 0 }

loopUnrollHelp :: String
loopUnrollHelp =  "Number of loop unrollings to performper loop"

debugModeHelp :: String
debugModeHelp = "Prints out the source changes at each step"
