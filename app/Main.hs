{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Data.List              (isSuffixOf, sort, delete)
import           Data.List.Extra        (dropSuffix)
import           Data.Text              (Text)
import qualified Data.Text              as T (isInfixOf, pack, toLower, toLower)
import           Data.Text.IO           as TIO (appendFile, putStrLn, readFile,
                                                writeFile)
import           Data.Time.Clock        (getCurrentTime)
import           Network.Socket         (SockAddr (..))
import           Network.Wai            (Request (remoteHost))
import           PageName               (PageName, asString, asText,
                                         pageName, wikiWordToMdLink,
                                         homePage, recentChanges)
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
import           Formatting             (string, (%), format )
import           Data.Text.Lazy         (toStrict)
import           Data.Maybe             (isJust, fromMaybe)

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
getHomeR = getPageR homePage 

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

-- | Handler for GET /#PageName
getPageR :: PageName -> Handler Html
getPageR pageName = do
  path <- getDocumentRoot                            -- obtain path to document root 
  maybeShowRefs <- lookupGetParam "showBackrefs"     -- check whether URL ends with '?showBackrefs'
  maybeBackrefs <- liftIO $ 
    computeMaybeBackrefs path pageName maybeShowRefs -- if showBackrefs was set, Just [PageName] else Nothing
  let fileName = fileNameFor path pageName           -- compute proper filename from pageName
  exists <- liftIO $ doesFileExist fileName          -- check whether such a file exists
  if exists
    then do                                                                  
      content <- liftIO $ TIO.readFile fileName      -- file exists, read its content
      return $ buildViewFor 
        pageName content maybeBackrefs               -- build HTML for content and return it
    else do
      redirect $ EditR pageName                      -- file does not exist, redirect to EditR

-- | handler for GET /edit/#PageName
getEditR :: PageName -> Handler Html
getEditR pageName = do
  path <- getDocumentRoot                    -- obtain path to document root 
  let fileName = fileNameFor path pageName   -- construct a file from the page name
  exists <- liftIO $ doesFileExist fileName  -- check whether file already exists
  markdown <-
    if exists
      then liftIO $ TIO.readFile fileName    -- if file exists, assign markdown with file content
      else return newPage                    -- else assign markdown with some default content
  return $ buildEditorFor pageName markdown  -- build Html for an Editor page, fill it with markdown content

postEditR :: PageName -> Handler Html
postEditR pageName = do
  path <- getDocumentRoot                    -- obtain path to document root
  let fileName = fileNameFor path pageName   -- construct a file from the page name
  maybeContent <- lookupPostParam "content"  -- retrieve POST data
  client <- remoteHost <$> waiRequest        -- retrieve info on remote client from request
  case maybeContent of
    Just content -> liftIO $ do
      TIO.writeFile fileName content         -- if content exists write it to disk
      writeLogEntry path pageName client     -- also write a log entry to file RecentChanges
    Nothing -> return ()                     -- no content: do nothing
  redirect $ PageR pageName                  -- redirect to GET Page route (display content)

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
      markMatches <- liftIO $ mapM (\p -> fmap containsSearchText $ return (asText p) <> TIO.readFile (fileNameFor path p)) allPages
      let pageBoolPairs = zip allPages markMatches
      let matchingPages = map fst (filter snd pageBoolPairs)
      return $ buildFindPage search matchingPages

writeLogEntry :: FilePath -> PageName -> SockAddr -> IO ()
writeLogEntry path pageName client = do
  let logFile = fileNameFor path recentChanges 
  now <- getCurrentTime
  let logEntry = toStrict $
        format ("- " % string % " " % string % " from " % string % "\n")
          (asString pageName)
          (takeWhile (/= '.') (show now))
          (takeWhile (/= ':') (show client))
  TIO.appendFile logFile logEntry

-- helper functions

-- | retrieve the name of the HsWiki {contentDir} attribute, defaults to 'content'
getDocumentRoot :: Handler String
getDocumentRoot = getsYesod contentDir

-- | construct the proper file name for a PageName
fileNameFor :: FilePath -> PageName  -> FilePath
fileNameFor path pageName = path ++ "/" ++ asString pageName ++ ".md"

computeIndex :: FilePath -> IO [PageName]
computeIndex path = do
  allFiles <- listDirectory path
  let pages = sort $ map (dropSuffix ".md") $ removeAll ["touch", "favicon.ico.md", "RecentChanges.md"] allFiles
  return $ map (fromMaybe  undefined) $ filter isJust $ map (pageName . T.pack) pages

-- | compute a list of all pages that contain references to pageName
computeBackRefs :: FilePath -> PageName -> [PageName] -> IO [PageName]
computeBackRefs path pageName allPages = do
  let filteredPages = delete pageName allPages   -- filter pagename from list of pages (avoid self refs)
  markRefs <- mapM                               -- create a list of bools: True if a page contains a ref,
    (fmap containsBackref . TIO.readFile . fileNameFor path) -- else False
    filteredPages
  let pageBoolPairs = zip filteredPages markRefs -- create a zipped list of (pageName, Bool) pairs
  return $ map fst (filter snd pageBoolPairs)    -- return only pages marked True
  where
    containsBackref content =                    -- returns True if content contains pageName, else False
      asText pageName `T.isInfixOf` content

-- | if maybeShowRefs isJust then a list of a pages referencing pageName is computed
computeMaybeBackrefs :: FilePath -> PageName -> Maybe Text -> IO (Maybe [PageName])
computeMaybeBackrefs path pageName maybeShowRefs =
  case maybeShowRefs of
    Nothing -> return Nothing                            -- if maybeShowRefs == Nothing, return Nothing
    Just _  -> do                                        -- else compute list of all references to page by
      allPages <- computeIndex path                      -- computing list of all pages in wiki
      backrefs <- computeBackRefs path pageName allPages -- compute all back references
      return $ Just backrefs                             -- return this list wrapped as a Maybe

removeAll :: (Foldable t, Eq a) => t a -> [a] -> [a]
removeAll = flip (foldl (flip remove))
  where
    remove :: Eq a => a -> [a] -> [a]
    remove = filter . (/=)