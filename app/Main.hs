{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import qualified Data.Text                     as T
import           Data.Text.Lazy
import           System.Directory              (doesFileExist)
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Markdown                 as MD (def, markdown)
import           Util.Config                   (getCommandLineArgs, dir, port)
import           Yesod

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
    Just content -> liftIO $ writeFile fileName $ T.unpack content
  redirect $ PageR page

-- helper functions
getDocumentRoot :: Handler String
getDocumentRoot = getsYesod contentDir

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
        md ++ "</textarea><button>save</button></form>"
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
  ") | &nbsp;&nbsp;&nbsp;&nbsp; built with [HsWiki](https://github.com/thma/HsWiki) | \r\n\r\n" ++
  "#" ++ unpack page ++ "\r\n"

menuBar :: Text -> Html
menuBar page = parseToMd $ mdMenu page

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
buildViewFor page content = toHtml [pageHeader, menuBar page, parseToMd content, pageFooter]

parseToMd :: String -> Html
parseToMd s = MD.markdown MD.def $ pack s