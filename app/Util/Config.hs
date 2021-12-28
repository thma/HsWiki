{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Util.Config
  ( getCommandLineArgs,
    port,
    dir,
  )
where

import           System.Console.CmdArgs

data Args = Args
  { port :: Int,
    dir  :: String
  }
  deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs =
  Args
    { port = 3000 &= help "IP port to be used",
      dir =
        "content"
          &= typ "DIRECTORY"
          &= help "store pages in this directory"
    }
    &= summary "HsWiki v1.0, https://github.com/thma/HsWiki"
    &= program "HsWiki"

getCommandLineArgs :: IO Args
getCommandLineArgs = cmdArgs defaultArgs
