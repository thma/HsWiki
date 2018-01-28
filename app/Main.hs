{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Main where

import qualified Data.Text                     as T
import           Data.Text.Lazy
import           System.Directory              (doesFileExist)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Markdown                 (def, markdown)
import           Yesod
import           Yesod.Default.Config

data HsWiki = HsWiki

mkYesod "HsWiki" [parseRoutes|
/           HomeR   GET
/#Text      OtherR  GET
/edit/#Text EditR   GET POST
|]

instance Yesod HsWiki

getEditR :: Text -> Handler TypedContent
getEditR page = do
    editor <- liftIO $ editorFor page
    sendResponse $ TypedContent typeHtml $ toContent editor


editorFor :: Text -> IO String
editorFor page = do
    let fileName = fileNameFor page
    md <- liftIO $ readFile fileName
    return $ "<form action=\"" ++ unpack page ++ "\" method=\"POST\">" ++
                "<textarea name=\"content\" cols=\"120\" rows=\"40\">" ++
                md ++
                "</textarea><button>save</button></form>"

fileNameFor :: Text -> String
fileNameFor page =
    --conf <- loadloadDevelopmentConfig
    "content/" ++ unpack page ++ ".md"

postEditR :: Text -> Handler Html
postEditR page = do
    let fileName = fileNameFor page
    maybeContent <- lookupPostParam "content"
    case maybeContent of
        Just content -> liftIO $ writeFile fileName $ T.unpack content
    redirect $ OtherR page

getHomeR :: Handler Html
getHomeR = getOtherR $ pack "index"

getOtherR :: Text -> Handler Html
getOtherR page = do
    let pageName = unpack page
    let fileName = fileNameFor page
    exists <- liftIO $ doesFileExist fileName
    if exists then do
        content <- liftIO $ readFile fileName
        return $ fullPage pageName content
    else do
        let content = newPage
        liftIO $ writeFile fileName content
        return $ fullPage pageName content

newPage :: String
newPage = "\r\nUse MarkDown to format page content"

pageHeader :: String -> String
pageHeader page =
        "[home](index) | versions | referenced by | [edit](edit/" ++ page ++ ") | \r\n\r\n" ++
        "#" ++ page ++ "\r\n"

fullPage :: String -> String -> Html
fullPage page content = markdown def $ pack $ pageHeader page ++ content

main :: IO ()
main = warp 3000 HsWiki
