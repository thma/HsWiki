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
import           Text.Markdown                 (def, markdown)
import           Yesod

data HsWiki = HsWiki

mkYesod "HsWiki" [parseRoutes|
/           HomeR   GET
/#Text      PageR   GET
/edit/#Text EditR   GET POST
|]

instance Yesod HsWiki

main :: IO ()
main = warp 3000 HsWiki

-- Route Handlers
getHomeR :: Handler Html
getHomeR = getPageR "index"

getPageR :: Text -> Handler Html
getPageR page = do
    let fileName = fileNameFor page
    exists <- liftIO $ doesFileExist fileName
    if exists then do
        content <- liftIO $ readFile fileName
        return $ htmlFromMd page content
    else do
        let content = newPage
        liftIO $ writeFile fileName content
        redirect $ EditR page

getEditR :: Text -> Handler Html
getEditR page = do
    editor <- liftIO $ editorFor page
    return $ preEscapedToHtml editor

postEditR :: Text -> Handler Html
postEditR page = do
    let fileName = fileNameFor page
    maybeContent <- lookupPostParam "content"
    case maybeContent of
        Just content -> liftIO $ writeFile fileName $ T.unpack content
    redirect $ PageR page

-- helper functions
editorFor :: Text -> IO String
editorFor page = do
    let fileName = fileNameFor page
    md <- liftIO $ readFile fileName
    return $ unpack (renderHtml cssLink) ++ htmlHeader page ++
        "<form action=\"" ++ unpack page ++ "\" method=\"POST\">" ++
            "<textarea name=\"content\" cols=\"120\" rows=\"30\">" ++
            md ++
            "</textarea><button>save</button></form>"

fileNameFor :: Text -> String
fileNameFor page =
    "content/" ++ unpack page ++ ".md"

newPage :: String
newPage = "\r\n## Use MarkDown to format page content"

mdHeader :: String -> String
mdHeader page =
        "[home](/) | versions | referenced by | [edit](/edit/" ++ page ++ ") | \r\n\r\n" ++
        "#" ++ page ++ "\r\n"

htmlHeader :: Text -> String
htmlHeader page =
  (unpack $ renderHtml $ markdown def $ pack $ mdHeader (unpack page))

cssLink :: Html
cssLink = preEscapedToHtml (
  "<link rel=\"stylesheet\" href=\"//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\">" ++
  "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/necolas/normalize.css/master/normalize.css\">" ++
  "<link rel=\"stylesheet\" href=\"//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css\">" :: String)

htmlFromMd :: Text -> String -> Html
htmlFromMd page content =
  toHtml [cssLink, markdown def $ pack $ mdHeader (unpack page) ++ content]
