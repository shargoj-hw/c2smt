{-# LANGUAGE DeriveDataTypeable #-}

module Config where

import           Data.Data
import           System.Console.CmdArgs.Implicit


defaultConfig :: Config
defaultConfig =
  Config { loopUnrolls = 10
                         &= help "Number of loop unrollings to performper loop"
         , fileName = def &= argPos 0
  }


data Config =
  Config
  {
    loopUnrolls :: Integer -- Number of loop unrollings to perform
  , fileName    :: String
  }
  deriving (Show, Data, Typeable)
