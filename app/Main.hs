{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Data.List                       (isSuffixOf, sort)
import           Data.List.Extra                 (dropSuffix, isInfixOf)
import qualified Data.Text                       as T (unpack)
import           Data.Text.Lazy                  (Text, pack, unpack)
import           System.Console.CmdArgs          ()
import           System.Directory                (doesFileExist, listDirectory)
import           Util.Config                     (dir, getCommandLineArgs, port)
import           Util.HtmlElements               (buildBackRefs, buildEditorFor,
                                                  buildIndex, buildViewFor,
                                                  buildGraphView, newPage)
import           Yesod                           (Html, MonadIO (liftIO),
                                                  RenderRoute (renderRoute),
                                                  Yesod, getsYesod,
                                                  lookupPostParam, mkYesod,
                                                  parseRoutes, redirect, warp, waiRequest)
import           Data.Time.Clock                 (getCurrentTime )
import           Network.Wai
import           Network.Socket                  (SockAddr(..) )

newtype HsWiki = HsWiki
  { contentDir :: String
  }

mkYesod "HsWiki" [parseRoutes|
/                       HomeR     GET
/#Text                  PageR     GET
/edit/#Text             EditR     GET POST
/actions/backref/#Text  BackRefR  GET
/actions/graph          GraphR    GET
/actions/toc            IndexR    GET
|]

instance Yesod HsWiki

main :: IO ()
main = do
  args <- getCommandLineArgs
  putStrLn $ "HsWiki starting on port " ++ show (port args) ++ ", document root: " ++ dir args
  warp (port args) HsWiki {contentDir = dir args}

-- Route Handlers
getHomeR :: Handler Html
getHomeR = getPageR "home"

getIndexR :: Handler Html
getIndexR = do
  path <- getDocumentRoot
  index <- liftIO $ computeIndex path
  return $ buildIndex index

getBackRefR :: Text -> Handler Html
getBackRefR page = do
  path <- getDocumentRoot
  allPages <- liftIO $ computeIndex path
  backRefs <- liftIO $ computeBackRefs path page allPages
  return $ buildBackRefs page backRefs

getGraphR :: Handler Html
getGraphR = do
  path <- getDocumentRoot
  allPages <- liftIO $ computeIndex path
  allRefs  <- liftIO $  mapM (\p -> computeBackRefs path (pack p) allPages) allPages
  return $ buildGraphView $ zip allRefs allPages

getPageR :: Text -> Handler Html
getPageR page = do
  path <- getDocumentRoot
  let fileName = fileNameFor path page
  exists <- liftIO $ doesFileExist fileName
  if exists
    then do
      content <- liftIO $ readFile fileName
      return $ buildViewFor page content
    else do
      redirect $ EditR page

getEditR :: Text -> Handler Html
getEditR page = do
  path <- getDocumentRoot
  let fileName = fileNameFor path page
  exists <- liftIO $ doesFileExist fileName
  md <-
    if exists
      then liftIO $ readFile $ fileNameFor path page
      else return $ newPage page
  return $ buildEditorFor page md

postEditR :: Text -> Handler Html
postEditR page = do
  path <- getDocumentRoot
  let fileName = fileNameFor path page
  maybeContent <- lookupPostParam "content"
  client <- remoteHost <$> waiRequest
  case maybeContent of
    Just content -> liftIO $ do 
      writeFile fileName $ T.unpack content
      writeLogEntry path page client
    Nothing      -> redirect $ PageR page
  redirect $ PageR page
  
writeLogEntry :: FilePath -> Text -> SockAddr -> IO ()
writeLogEntry path page client = do
  let logFile = fileNameFor path "RecentChanges"
  now <- getCurrentTime
  let logEntry = "- [" ++ unpack page ++ "](" ++ unpack page ++ ") " ++ 
        takeWhile (/= '.') (show now) ++ " from " ++ 
        takeWhile (/= ':') (show client) ++ "\n"
  appendFile logFile logEntry

-- helper functions
getDocumentRoot :: Handler String
getDocumentRoot = getsYesod contentDir

fileNameFor :: FilePath -> Text -> String
fileNameFor path page =
  let p = unpack page
   in path ++ "/" ++ p
        ++ if ".md~" `isSuffixOf` p
          then ""
          else ".md"

computeIndex :: FilePath -> IO [String]
computeIndex path = do
  allFiles <- listDirectory path
  let pages = removeAll ["touch", "favicon.ico.md", "RecentChanges.md"] allFiles
  return $ sort $ map (dropSuffix ".md") pages

computeBackRefs :: String -> Text -> [String] -> IO [String]
computeBackRefs path page allPages = do
  markRefs <- mapM (fmap containsBackref . readFile . fileNameFor path . pack) allPages
  let pageBoolPairs = zip allPages markRefs
  return $ map fst (filter snd pageBoolPairs)
  where
    containsBackref content = ("](" ++ unpack page ++ ")") `isInfixOf` content

removeAll :: (Foldable t, Eq a) => t a -> [a] -> [a]
removeAll = flip (foldl (flip remove))
  where
    remove :: Eq a => a -> [a] -> [a]
    remove = filter . (/=)
