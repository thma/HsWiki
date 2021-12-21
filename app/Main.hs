{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import qualified Data.Text                     as T
import           Data.Text.Lazy                (Text, pack, unpack)
import           Data.List
import           System.Directory              (doesFileExist, listDirectory)
import           Text.Blaze.Html               ( preEscapedToHtml, toHtml, Html )
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           CMarkGFM                      (commonmarkToHtml)
import           Control.Monad                 (when)
import           System.Console.CmdArgs
import           Yesod
import Util.Config

newtype HsWiki = HsWiki
  { contentDir :: String
  }

mkYesod "HsWiki" [parseRoutes|
/           HomeR   GET
/#Text      PageR   GET
/edit/#Text EditR   GET POST
|]

instance Yesod HsWiki

main :: IO ()
main = do
  args <- getCommandLineArgs
  putStrLn $ "HsWiki starting on port " ++ show (port args) ++ ", document root: " ++ dir args
  warp (port args) HsWiki {contentDir = dir args}

-- Route Handlers
getHomeR :: Handler Html
getHomeR = getPageR "index"

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
  liftIO $ buildEditorFor path page

postEditR :: Text -> Handler Html
postEditR page = do
  path <- getDocumentRoot
  let fileName = fileNameFor path page
  maybeContent <- lookupPostParam "content"
  case maybeContent of
    Just content -> liftIO $ do
      safeVersion path page
      writeFile fileName $ T.unpack content
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
    let newFileName = fileNameFor path $ pack (unpack page ++ version)
    writeFile newFileName content

maxVersion :: FilePath -> String -> IO String
maxVersion path page = do
  allFiles <- listDirectory path
  let versions = length $ filter (isPrefixOf page) allFiles
  return $ "." ++ show versions

buildEditorFor :: FilePath -> Text -> IO Html
buildEditorFor path page = do
  md <- readFile $ fileNameFor path page
  return $
    toHtml
      [ pageHeader
      , menuBar page
      , preEscapedToHtml $
        "<form action=\"" ++
          unpack page ++
          "\" method=\"POST\">" ++
          "<textarea style=\"height: auto;\" name=\"content\" cols=\"120\" rows=\"25\">" ++
          md ++ "</textarea>" ++
          "<input type=\"submit\" name=\"save\" value=\"save\" /> &nbsp; " ++
          "<input type=\"button\" name=\"cancel\" value=\"cancel\" onClick=\"window.location.href='/" ++ unpack page ++ "';\" />" ++     
        "</form>"
      , pageFooter
      ]

fileNameFor :: FilePath -> Text -> String
fileNameFor path page = path ++ "/" ++ unpack page ++ ".md"

newPage :: String
newPage = "Use [MarkDown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) to format page content"

mdMenu :: Text -> String
mdMenu page =
  "[home](/) | versions | referenced by | [edit](/edit/" ++
  unpack page ++
  ") | &nbsp;&nbsp;&nbsp;&nbsp; built with [HsWiki](https://github.com/thma/HsWiki) \r\n\r\n" ++
  "# " ++ unpack page ++ "\r\n"

menuBar :: Text -> Html
menuBar page = renderMdToHtml $ mdMenu page

pageHeader :: Html
pageHeader =
  preEscapedToHtml $
  "<!DOCTYPE html>\r\n<html>\r\n<head>\r\n" ++
  "<title>HsWiki</title>\r\n" ++
  "<meta charset=\"UTF-8\">\r\n" ++
  "<link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\">\r\n" ++
  "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/necolas/normalize.css/master/normalize.css\">\r\n" ++
  "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css\">\r\n" ++
  "</head>\r\n<body>\r\n<div class=\"container\">\r\n"

pageFooter :: Html
pageFooter = preEscapedToHtml ("\r\n</div></body></html>" :: String)

buildViewFor :: Text -> String -> Html
buildViewFor page content = toHtml [pageHeader, menuBar page, renderMdToHtml content, pageFooter]

renderMdToHtml :: String -> Html
renderMdToHtml s = preEscapedToHtml $ commonmarkToHtml [] [] $ T.pack s