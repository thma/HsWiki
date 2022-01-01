{-# LANGUAGE OverloadedStrings #-}

module Util.HtmlElements
  ( buildViewFor,
    buildEditorFor,
    buildIndex,
    buildBackRefs,
    buildGraphView,
    newPage,
  )
where

import           CMarkGFM                      (commonmarkToHtml)
import qualified Data.Text                     as T (pack, unpack)
import           Data.Text                     (Text)
import           Text.Blaze.Html               (Html, preEscapedToHtml, toHtml)
import           Data.List                     (nub)

menuBar :: Text -> Html
menuBar page = renderMdToHtml $ mdMenu page

pageHeader :: Html
pageHeader =
  preEscapedToHtml $
    "<!DOCTYPE html>\r\n<html>\r\n<head>\r\n"
      ++ "<title>HsWiki</title>\r\n"
      ++ "<meta charset=\"UTF-8\">\r\n"
      ++ "<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\"> \n"
      ++ "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css\"> \n"
      ++ "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css\"> \n"
      ++ "</head>\r\n<body>\r\n<div class=\"container\">\r\n"

pageFooter :: Html
pageFooter = preEscapedToHtml ("\r\n</div></body></html>" :: String)

buildViewFor :: Text -> Text -> Html
buildViewFor page content = toHtml [pageHeader, menuBar page, renderMdToHtml content, pageFooter]

buildEditorFor :: Text -> Text -> Html
buildEditorFor page markdown =
  toHtml
    [ pageHeader,
      menuBar page,
      preEscapedToHtml $
        "<form action=\""
          ++ T.unpack page
          ++ "\" method=\"POST\">"
          ++ "<textarea style=\"height: auto;\" name=\"content\" cols=\"120\" rows=\"25\">"
          ++ T.unpack markdown
          ++ "</textarea>"
          ++ "<input type=\"submit\" name=\"save\" value=\"save\" /> &nbsp; "
          ++ "<input type=\"button\" name=\"cancel\" value=\"cancel\" onClick=\"window.history.back()\" /> "
          ++ "</form>",
      pageFooter
    ]

buildIndex :: [String] -> Html
buildIndex index =
  toHtml
    [ pageHeader,
      menuBar "",
      renderMdToHtml "# Table of contents \n",
      renderMdToHtml $ T.pack $ concatMap (\page -> "- [" ++ page ++ "](/" ++ page ++ ") \n") index,
      pageFooter
    ]

buildBackRefs :: FilePath -> [String] -> Html
buildBackRefs page backrefs =
  toHtml
    [ pageHeader,
      menuBar "",
      renderMdToHtml $ T.pack $ "# References to " ++ page ++ "\n",
      renderMdToHtml $ T.pack $ concatMap (\b -> "- [" ++ b ++ "](/" ++ b ++ ") \n") backrefs,
      pageFooter
    ]

buildGraphView :: [([String],String)] -> Html
buildGraphView graph =
  toHtml
    [ pageHeader,
      menuBar "",
      renderMdToHtml "# Site Map \n",
      renderMdToHtml "[View as List](/actions/toc) \n",
      top,
      preEscapedToHtml $ renderNodes $ allNodes graph,
      preEscapedToHtml $ renderGraph graph,
      bottom,
      pageFooter
    ]


renderGraph :: [([String], String)] -> String
renderGraph graph =
  foldr (\str -> ((str ++ ",\n") ++)) ""
    (concatMap (\(sources, target) -> map (\s -> "'\"" ++ s ++ "\" -> \"" ++ target ++ "\";'") sources) graph)

allNodes :: [([String], String)] -> [String]
allNodes = nub . (uncurry (flip (:)) =<<)

renderNodes :: [String] -> String
renderNodes =
  concatMap (\n -> "'\"" ++ n ++
    "\" [shape=\"rect\", style=\"rounded,filled\", fillcolor=\"#f4f5f6\", fontcolor=\"#9b4dca\", fontname=\"Roboto\",  URL=\"/" ++
    n ++ "\"];', \n")

top :: Html
top = preEscapedToHtml $
      "<script src=\"//d3js.org/d3.v5.min.js\"></script>" ++
      "<script src=\"https://unpkg.com/@hpcc-js/wasm@0.3.11/dist/index.min.js\"></script>" ++
      "<script src=\"https://unpkg.com/d3-graphviz@3.0.5/build/d3-graphviz.js\"></script>" ++
      "<div id=\"graph\" ></div>" ++
      "<script>" ++
      "var dot =\n" ++
      "    [\n" ++
      "        'digraph  {',\n"

bottom = preEscapedToHtml $ "        '}'\n" ++
         "     ];\n" ++
         " \n" ++
         " d3.select(\"#graph\").graphviz()\n" ++
         "     .renderDot(dot.join(''));\n" ++
         " \n" ++
         " </script>\n"

renderMdToHtml :: Text -> Html
renderMdToHtml = preEscapedToHtml . commonmarkToHtml [] [] 

newPage :: Text -> Text
newPage page = T.pack $
  "# " ++ T.unpack page ++ "\n\n"
    ++ "### [Be the first to edit this page](/edit/"
    ++ T.unpack page
    ++ ") \n\n"
    ++ "Use [MarkDown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) to format page content"

mdMenu :: Text -> Text
mdMenu page = T.pack $
  "[home](/) | [site map](/actions/graph) |  [recent changes](/RecentChanges) | "
    ++ ( if page == ""
           then "referenced by | edit"
           else "[referenced by](/actions/backref/" ++ T.unpack page ++ ") | [edit](/edit/" ++ T.unpack page ++ ")"
       )
    ++ " | &nbsp;&nbsp;&nbsp;&nbsp; built with [HsWiki](https://github.com/thma/HsWiki) \r\n\r\n"
