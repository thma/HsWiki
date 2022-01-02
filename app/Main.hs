{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Data.List              (isSuffixOf, sort)
import           Data.List.Extra        (dropSuffix, isInfixOf)
import           Data.Text              (Text)
import qualified Data.Text              as T (concat, isInfixOf, pack, unpack)
import           Data.Text.IO           as TIO (appendFile, putStrLn, readFile,
                                                writeFile)
import           Data.Time.Clock        (getCurrentTime)
import           Network.Socket         (SockAddr (..))
import           Network.Wai            (Request (remoteHost))
import           PageName               (PageName (..), asString, asText,
                                         wikiWordToMdLink)
import           System.Console.CmdArgs ()
import           System.Directory       (doesFileExist, listDirectory)
import           Util.Config            (dir, getCommandLineArgs, port)
import           Util.HtmlElements      (buildBackRefs, buildEditorFor,
                                         buildGraphView, buildIndex,
                                         buildViewFor, newPage)
import           Yesod                  (Html, MonadIO (liftIO),
                                         RenderRoute (renderRoute), Yesod,
                                         getsYesod, lookupGetParam,
                                         lookupPostParam, mkYesod, parseRoutes,
                                         redirect, waiRequest, warp)

newtype HsWiki = HsWiki
  { contentDir :: String
  }

mkYesod "HsWiki" [parseRoutes|
/               HomeR     GET
/#PageName      PageR     GET
/edit/#PageName EditR     GET POST
/actions/graph  GraphR    GET
/actions/toc    IndexR    GET
|]

instance Yesod HsWiki

main :: IO ()
main = do
  args <- getCommandLineArgs
  TIO.putStrLn $ T.pack $ "HsWiki starting on port " ++ show (port args) ++ ", document root: " ++ dir args
  warp (port args) HsWiki {contentDir = dir args}

-- Route Handlers
getHomeR :: Handler Html
getHomeR = getPageR (Page "HomePage")

getIndexR :: Handler Html
getIndexR = do
  path <- getDocumentRoot
  index <- liftIO $ computeIndex path
  return $ buildIndex index

getBackRefR :: PageName -> Handler Html
getBackRefR page = do
  let pageName = asString page
  path <- getDocumentRoot
  allPages <- liftIO $ computeIndex path
  backRefs <- liftIO $ computeBackRefs path pageName allPages
  return $ buildBackRefs pageName backRefs

getGraphR :: Handler Html
getGraphR = do
  path <- getDocumentRoot
  allPages <- liftIO $ computeIndex path
  allRefs <- liftIO $ mapM (\p -> computeBackRefs path p allPages) allPages
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
  let pageStr = asString page
      pageT = asText page
      fileName = fileNameFor path pageStr
  exists <- liftIO $ doesFileExist fileName
  md <-
    if exists
      then liftIO $ TIO.readFile fileName
      else return $ newPage pageT
  return $ buildEditorFor pageT md

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

writeLogEntry :: FilePath -> FilePath -> SockAddr -> IO ()
writeLogEntry path page client = do
  let logFile = fileNameFor path "RecentChanges"
  now <- getCurrentTime
  let logEntry =
        "- " ++ page ++ " "
          ++ takeWhile (/= '.') (show now)
          ++ " from "
          ++ takeWhile (/= ':') (show client)
          ++ "\n"
  TIO.appendFile logFile $ T.pack logEntry

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
      allFiles <- listDirectory path
      allPages <- computeIndex path
      backrefs <- computeBackRefs path page allPages
      return $ Just backrefs

-- computeForwardRefs :: FilePath -> FilePath -> IO [String]
-- computeForwardRefs path page = do
--   content <- TIO.readFile (fileNameFor path page)
--   let text = content
--   let node = commonmarkToNode [] [] text
--   print node
--   return undefined

removeAll :: (Foldable t, Eq a) => t a -> [a] -> [a]
removeAll = flip (foldl (flip remove))
  where
    remove :: Eq a => a -> [a] -> [a]
    remove = filter . (/=)
