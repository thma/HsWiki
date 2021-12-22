{-# LANGUAGE OverloadedStrings #-}
module Util.HtmlElements
  ( buildViewFor
  , buildEditorFor
  , buildIndex
  , buildVersions
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
  , renderMdToHtml $ concatMap (\page -> "- [" ++ page ++ "](/" ++ page ++ ") \n") index
  , pageFooter
  ]

buildVersions page versions = 
  toHtml 
  [
    pageHeader
  , menuBar ""  
  , renderMdToHtml $ concatMap (\v -> "- [" ++ v ++ "](/" ++ v ++ ") \n") versions
  , pageFooter
  ]


renderMdToHtml :: String -> Html
renderMdToHtml s = preEscapedToHtml $ commonmarkToHtml [] [] $ T.pack s

newPage :: String
newPage = "Use [MarkDown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) to format page content"

mdMenu :: Text -> String
mdMenu page =
  "[home](/) | [all pages](/actions/index) | [versions](/actions/versions/" ++ 
  unpack page ++ ") | referenced by | " ++ 
  (if page == ""
    then "edit" 
    else "[edit](/edit/" ++ unpack page ++ ")")
  ++ " | &nbsp;&nbsp;&nbsp;&nbsp; built with [HsWiki](https://github.com/thma/HsWiki) \r\n\r\n" ++
  "# " ++ unpack page ++ "\r\n"
