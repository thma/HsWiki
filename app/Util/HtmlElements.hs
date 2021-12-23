{-# LANGUAGE OverloadedStrings #-}
module Util.HtmlElements
  ( buildViewFor
  , buildEditorFor
  , buildIndex
  , buildBackRefs
  , newPage
  )
where

import           Text.Blaze.Html               ( preEscapedToHtml, toHtml, Html )
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text                     as T (pack, unpack)
import           Data.Text.Lazy                (Text, pack, unpack)
import           CMarkGFM                      (commonmarkToHtml)


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

buildEditorFor :: Text -> String -> Html
buildEditorFor page markdown =
    toHtml
      [ pageHeader
      , menuBar page
      , preEscapedToHtml $
        "<form action=\"" ++
          unpack page ++
          "\" method=\"POST\">" ++
          "<textarea style=\"height: auto;\" name=\"content\" cols=\"120\" rows=\"25\">" ++
          markdown ++ "</textarea>" ++
          "<input type=\"submit\" name=\"save\" value=\"save\" /> &nbsp; " ++
          "<input type=\"button\" name=\"cancel\" value=\"cancel\" onClick=\"window.location.href='/" ++ unpack page ++ "';\" />" ++
        "</form>"
      , pageFooter
      ]

buildIndex :: [String] -> Html
buildIndex index =
  toHtml 
  [
    pageHeader
  , menuBar ""  
  , renderMdToHtml "# All pages in this wiki \n"
  , renderMdToHtml $ concatMap (\page -> "- [" ++ page ++ "](/" ++ page ++ ") \n") index
  , pageFooter
  ]

buildBackRefs :: Text -> [String] -> Html
buildBackRefs page backrefs = 
  toHtml 
  [
    pageHeader
  , menuBar ""  
  , renderMdToHtml $ "# References to " ++ unpack page ++ "\n"
  , renderMdToHtml $ concatMap (\b -> "- [" ++ b ++ "](/" ++ b ++ ") \n") backrefs
  , pageFooter
  ]

renderMdToHtml :: String -> Html
renderMdToHtml s = preEscapedToHtml $ commonmarkToHtml [] [] $ T.pack s

newPage :: Text -> String
newPage page = 
  "[### Be the first to edit this page](/edit/" ++ unpack page ++ ") \n\n" ++
  "Use [MarkDown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) to format page content"

mdMenu :: Text -> String
mdMenu page =
  "[home](/) | [all pages](/actions/index) | " ++ 
  -- "[versions](/actions/versions/" ++ unpack page ++ ") | "  ++ 
  (if page == ""
    then "referenced by | edit" 
    else "[referenced by](/actions/backref/" ++  unpack page ++ ") | [edit](/edit/" ++ unpack page ++ ")")
  ++ " | &nbsp;&nbsp;&nbsp;&nbsp; built with [HsWiki](https://github.com/thma/HsWiki) \r\n\r\n" 
  -- ++ "# " ++ unpack page ++ "\r\n"
