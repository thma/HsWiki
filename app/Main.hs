{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import qualified Data.Text                     as T (unpack)
import           Data.Text.Lazy                (Text, pack, unpack)
import Data.List (isPrefixOf, isSuffixOf, sort )
import Data.List.Extra
import           System.Directory              (doesFileExist, listDirectory, pathIsSymbolicLink)
import           Control.Monad                 (when)
import System.Console.CmdArgs ()
import Yesod
    ( Html,
      warp,
      getsYesod,
      lookupPostParam,
      redirect,
      mkYesod,
      parseRoutes,
      MonadIO(liftIO),
      Yesod,
      RenderRoute(renderRoute) )
import Util.Config ( port, dir, getCommandLineArgs )
import Util.HtmlElements
    ( buildViewFor, buildEditorFor, buildIndex, newPage )

newtype HsWiki = HsWiki
  { contentDir :: String
  }

mkYesod "HsWiki" [parseRoutes|
/           HomeR   GET
/#Text      PageR   GET
/edit/#Text EditR   GET POST
/actions/index IndexR GET
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
      liftIO $ writeFile fileName newPage
      redirect $ EditR page

getEditR :: Text -> Handler Html
getEditR page = do
  path <- getDocumentRoot
  md <- liftIO $ readFile $ fileNameFor path page
  return $ buildEditorFor page md

postEditR :: Text -> Handler Html
postEditR page = do
  path <- getDocumentRoot
  let fileName = fileNameFor path page
  maybeContent <- lookupPostParam "content"
  case maybeContent of
    Just content -> liftIO $ do
      safeVersion path page
      writeFile fileName $ T.unpack content
    Nothing -> redirect $ PageR page
  redirect $ PageR page

-- helper functions
getDocumentRoot :: Handler String
getDocumentRoot = getsYesod contentDir

safeVersion :: FilePath -> Text -> IO ()
safeVersion path page = do
  let fileName = fileNameFor path page
  exists <- doesFileExist fileName
  when exists $ do
    content <- readFile fileName
    version <- maxVersion path (unpack page)
    let newFileName = fileNameFor path (pack (unpack page ++ version)) ++ "~"
    writeFile newFileName content

maxVersion :: FilePath -> String -> IO String
maxVersion path page = do
  allFiles <- listDirectory path
  let versions = length $ filter (isPrefixOf page) allFiles
  return $ "." ++ show versions

fileNameFor :: FilePath -> Text -> String
fileNameFor path page = path ++ "/" ++ unpack page ++ ".md"

computeIndex :: FilePath -> IO [String]
computeIndex path = do
  allFiles <- listDirectory path
  let filteredPages = filter (not .isSuffixOf ".md~") allFiles
  let pages = removeAll ["touch", "favicon.ico.md"] filteredPages

  return $ sort $ map (dropSuffix ".md") pages

removeAll :: (Foldable t, Eq a) => t a -> [a] -> [a]
removeAll es list = foldl (flip remove) list es

remove :: Eq a => a -> [a] -> [a]
remove element = filter (/= element)


