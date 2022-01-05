{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Data.List              (isSuffixOf, sort)
import           Data.List.Extra        (dropSuffix)
import           Data.Text              (Text)
import qualified Data.Text              as T (isInfixOf, pack, toLower, toLower)
import           Data.Text.IO           as TIO (appendFile, putStrLn, readFile,
                                                writeFile)
import           Data.Time.Clock        (getCurrentTime)
import           Network.Socket         (SockAddr (..))
import           Network.Wai            (Request (remoteHost))
import           PageName               (PageName, asString, asText,
                                         pageName, wikiWordToMdLink)
import           System.Console.CmdArgs ()
import           System.Directory       (doesFileExist, listDirectory)
import           Util.Config            (dir, getCommandLineArgs, port)
import           Util.HtmlElements      (buildEditorFor, buildFindPage,
                                         buildGraphView, buildIndex,
                                         buildViewFor, newPage)
import           Yesod                  (Html, MonadIO (liftIO),
                                         RenderRoute (renderRoute), Yesod,
                                         getsYesod, lookupGetParam,
                                         lookupPostParam, mkYesod, parseRoutes,
                                         redirect, waiRequest, warp)
import           Formatting
import           Data.Text.Lazy         (toStrict)

newtype HsWiki = HsWiki
  { contentDir :: String
  }

mkYesod "HsWiki" [parseRoutes|
/               HomeR     GET
/#PageName      PageR     GET        -- (1)
/edit/#PageName EditR     GET POST   -- (2)
/actions/graph  GraphR    GET
/actions/toc    IndexR    GET
/actions/find/  FindR     GET
|]

instance Yesod HsWiki

main :: IO ()
main = do
  args <- getCommandLineArgs
  TIO.putStrLn $ T.pack $ "HsWiki starting on port " ++ show (port args) ++ ", document root: " ++ dir args
  warp (port args) HsWiki {contentDir = dir args}

-- Route Handlers
getHomeR :: Handler Html
getHomeR = 
  case pageName "HomePage" of
    Just page -> getPageR page
    Nothing   -> error "will not happen as HomePage is a WikiWord"


getIndexR :: Handler Html
getIndexR = do
  path  <- getDocumentRoot
  index <- liftIO $ computeIndex path
  return $ buildIndex index

getGraphR :: Handler Html
getGraphR = do
  path     <- getDocumentRoot
  allPages <- liftIO $ computeIndex path
  allRefs  <- liftIO $ mapM (\p -> computeBackRefs path p allPages) allPages
  return $ buildGraphView $ zip allRefs allPages

getPageR :: PageName -> Handler Html
getPageR page = do
  path <- getDocumentRoot
  maybeShowRefs <- lookupGetParam "showBackrefs"
  maybeBackrefs <- liftIO $ computeMaybeBackrefs path (asString page) maybeShowRefs
  let fileName = fileNameFor path (asString page)
  exists <- liftIO $ doesFileExist fileName
  if exists
    then do
      content <- liftIO $ TIO.readFile fileName
      return $ buildViewFor (asText page) (wikiWordToMdLink content) maybeBackrefs
    else do
      redirect $ EditR page

getEditR :: PageName -> Handler Html
getEditR page = do
  path <- getDocumentRoot
  let fileName = fileNameFor path (asString page)
  exists <- liftIO $ doesFileExist fileName
  md <-
    if exists
      then liftIO $ TIO.readFile fileName
      else return newPage
  return $ buildEditorFor (asText page) md

postEditR :: PageName -> Handler Html
postEditR page = do
  path <- getDocumentRoot
  let pageStr = asString page
  let fileName = fileNameFor path pageStr
  maybeContent <- lookupPostParam "content"
  client <- remoteHost <$> waiRequest
  case maybeContent of
    Just content -> liftIO $ do
      TIO.writeFile fileName content
      writeLogEntry path pageStr client
    Nothing -> redirect $ PageR page
  redirect $ PageR page
  
getFindR :: Handler Html
getFindR = do
  path <- getDocumentRoot
  allPages <- liftIO $ computeIndex path
  maybeSearch <- lookupGetParam "search"
  case maybeSearch of
    Nothing     -> return $ buildFindPage "" []
    Just ""     -> return $ buildFindPage "" []
    Just search -> do
      let containsSearchText content = T.toLower search `T.isInfixOf` T.toLower content
      markMatches <- liftIO $ mapM (\p -> fmap containsSearchText $ return (T.pack p) <> TIO.readFile (fileNameFor path p)) allPages
      let pageBoolPairs = zip allPages markMatches
      let matchingPages = map fst (filter snd pageBoolPairs)
      return $ buildFindPage search matchingPages

writeLogEntry :: FilePath -> FilePath -> SockAddr -> IO ()
writeLogEntry path page client = do
  let logFile = fileNameFor path "RecentChanges"
  now <- getCurrentTime  
  let logEntry = toStrict $ 
        format ("- " % string % " " % string % " from " % string % "\n")
          page
          (takeWhile (/= '.') (show now))
          (takeWhile (/= ':') (show client))  
  TIO.appendFile logFile logEntry

-- helper functions
getDocumentRoot :: Handler String
getDocumentRoot = getsYesod contentDir

fileNameFor :: FilePath -> FilePath -> String
fileNameFor path page =
  path ++ "/" ++ page
    ++ if ".md~" `isSuffixOf` page
      then ""
      else ".md"

computeIndex :: FilePath -> IO [String]
computeIndex path = do
  allFiles <- listDirectory path
  let pages = removeAll ["touch", "favicon.ico.md", "RecentChanges.md"] allFiles
  return $ sort $ map (dropSuffix ".md") pages

computeBackRefs :: FilePath -> FilePath -> [String] -> IO [String]
computeBackRefs path page allPages = do
  let filteredPages = filter (page /=) allPages
  markRefs <- mapM (fmap containsBackref . TIO.readFile . fileNameFor path) filteredPages
  let pageBoolPairs = zip filteredPages markRefs
  return $ map fst (filter snd pageBoolPairs)
  where
    containsBackref content = T.pack page `T.isInfixOf` content

computeMaybeBackrefs :: FilePath -> FilePath -> Maybe Text -> IO (Maybe [String])
computeMaybeBackrefs path page maybeShowRefs =
  case maybeShowRefs of
    Nothing -> return Nothing
    Just _ -> do
      allPages <- computeIndex path
      backrefs <- computeBackRefs path page allPages
      return $ Just backrefs

removeAll :: (Foldable t, Eq a) => t a -> [a] -> [a]
removeAll = flip (foldl (flip remove))
  where
    remove :: Eq a => a -> [a] -> [a]
    remove = filter . (/=)