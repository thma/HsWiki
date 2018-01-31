module Util.Helper (
    readFromArgsOrDefault
) where

import System.Environment (getArgs, withArgs)
import Text.Read          (readEither)

-- | if first command line argument exists and can be parsed to an Int -> return the Int value
-- | else return the provided default value d
readFromArgsOrDefault :: Int -> IO Int    
readFromArgsOrDefault d = do
    args <- getArgs
    case args of
        (p:_) -> do
            let intOrErr = readEither p :: Either String Int
            case intOrErr of
                Left _  -> return d
                Right p -> return p
        _     -> return d   
    