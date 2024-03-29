{-# LANGUAGE OverloadedStrings #-}

module Util.HtmlElements
  ( buildViewFor,
    buildEditorFor,
    buildIndex,
    buildGraphView,
    buildFindPage,
    newPage,
  )
where

import           CMarkGFM        (commonmarkToHtml)
import           Data.List       (nub)
import           Data.Text       (Text)
import qualified Data.Text       as T (pack, unpack, intercalate)
import           Text.Blaze.Html (Html, preEscapedToHtml, text, toHtml)
import PageName (PageName, asString, asText, wikiWordToMdLink)

menuBar :: Text -> Html
menuBar pageName = renderMdToHtml $ mdMenu pageName

pageHeader :: Bool -> Html
pageHeader renderInit =
  preEscapedToHtml (
    let initBlock = if renderInit then " onload=\"init()\"" else ""
    in
      "<!DOCTYPE html>\r\n<html>\r\n<head>\r\n"
        <> "<title>HsWiki</title>\r\n"
        <> "<meta charset=\"UTF-8\">\r\n"
        <> "<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic\"> \n"
        <> "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css\"> \n"
        <> "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css\"> \n"
        <> "</head>\r\n<body"
        <> initBlock
        <> ">\r\n<div class=\"container\">\r\n" :: Text)

pageFooter :: Html
pageFooter = preEscapedToHtml ("\r\n</div></body></html>" :: Text)

buildViewFor :: PageName -> Text -> Maybe [PageName] -> Html
buildViewFor pageName content maybeBackrefs =
  let (hasBackref, backrefEntry) = case maybeBackrefs of
        Nothing       -> (False, text "")
        Just backrefs -> (True, renderedBackrefs)
          where
            concatMap :: (a -> Text) -> [a] -> Text
            concatMap = (T.intercalate "" .) . map
            renderedBackrefs = renderMdToHtml $ concatMap ((\b -> "- [" <> b <> "](/" <> b <> ") \n") . asText) backrefs
   in toHtml [pageHeader False, 
              menuBar (asText pageName), 
              pageTitle pageName hasBackref, 
              backrefEntry, 
              renderMdToHtml (wikiWordToMdLink content), 
              pageFooter]

pageTitle :: PageName -> Bool -> Html
pageTitle pageName hasBackref =
  if hasBackref
    then renderMdToHtml $ "# [" <> pageT <> "](" <> pageT <> ")"
    else renderMdToHtml $ "# [" <> pageT <> "](" <> pageT <> "?showBackrefs)"
      where pageT = asText pageName

buildEditorFor :: PageName -> Text -> Html
buildEditorFor pageName markdown =
  toHtml
    [ pageHeader False,
      menuBar "",
      renderMdToHtml $ "# " <> page <> " \n",
      preEscapedToHtml $
        "<form action=\""
          <> page
          <> "\" method=\"POST\">"
          <> "<textarea style=\"height: auto;\" name=\"content\" cols=\"120\" rows=\"25\">"
          <> markdown
          <> "</textarea>"
          <> "<input type=\"submit\" name=\"save\" value=\"save\" /> &nbsp; "
          <> "<input class=\"button button-outline\" type=\"button\" name=\"cancel\" value=\"cancel\" onClick=\"window.history.back()\" /> "
          <> "</form>",
      pageFooter
    ]
  where page = asText pageName

buildIndex :: [PageName] -> Html
buildIndex index =
  toHtml
    [ pageHeader False,
      menuBar "",
      renderMdToHtml "# Table of contents \n",
      renderMdToHtml $ T.pack $ concatMap (\page -> "- [" ++ asString page ++ "](/" ++ asString page ++ ") \n") index,
      pageFooter
    ]

buildFindPage :: Text -> [PageName] -> Html
buildFindPage search pages = toHtml
  [ pageHeader True,
    menuBar "",
    renderMdToHtml "# FindPage ",
    searchBox search,
    renderMdToHtml $ T.pack $ concatMap (\p -> "- [" ++ asString p ++ "](/" ++ asString p ++ ") \n") pages,
    pageFooter
  ]

searchBox :: Text -> Html
searchBox search =
  preEscapedToHtml $
    "<script type=\"text/javascript\">"
    ++ "function init()"
    ++ "{"
    ++ "     document.getElementById(\"search\").focus();"
    ++ "}"
    ++ "</script>"
    ++
    "<form action=\"/actions/find\""
      ++ " method=\"GET\">"
      ++ "<input type=\"text\" id=\"search\" name=\"search\" value=\"" ++ T.unpack search ++ "\" "
      ++ "onfocus=\"this.setSelectionRange(9999, 9999)\" "
      ++ "onkeyup=\"this.form.submit()\" /> "
      ++ "<input type=\"submit\" value=\"find\" />"
      ++ "</form>"

-- | build view for GraphViz representation of wiki page structure
buildGraphView :: [([PageName], PageName)] -> Html
buildGraphView graph =
  toHtml
    [ pageHeader False,
      menuBar "",
      renderMdToHtml "# Site Map \n",
      renderMdToHtml "[View as List](/actions/toc) \n",
      preGraph,                                         -- load wasm scripts, begin JS script
      preEscapedToHtml $ renderNodes $ allNodes graph,  -- build list of all PageName nodes
      preEscapedToHtml $ renderGraph graph,             -- build link structure as directed graph
      postGraph,                                        -- render DOT digraph
      pageFooter
    ]

-- | render graph in DOT syntax (from -> to;)
renderGraph :: [([PageName], PageName)] -> String
renderGraph graph =
  foldr
    (\str -> ((str ++ ",\n") ++))
    ""
    (concatMap (\(sources, target) -> 
      map 
        (\s -> "'\"" ++ asString s ++ "\" -> \"" ++ asString target ++ "\";'") 
        sources) 
      graph)

-- | extract list of unique PageNames from graph
allNodes :: [([PageName], PageName)] -> [PageName]
allNodes = nub . (uncurry (flip (:)) =<<)

-- | render list of PageNames as DOT list of nodes with some nice formatting
renderNodes :: [PageName ] -> String
renderNodes =
  concatMap
    ( \n ->
        "'\"" ++ asString n
          ++ "\" [shape=\"rect\", style=\"rounded,filled\", fillcolor=\"#f4f5f6\", fontcolor=\"#9b4dca\", fontname=\"Roboto\",  URL=\"/"
          ++ asString n
          ++ "\"];', \n"
    )

-- | Html with script code for loading d3-graphviz and opening the DOT digraph
preGraph :: Html
preGraph =
  preEscapedToHtml $
    "<script src=\"//d3js.org/d3.v5.min.js\"></script>"
      ++ "<script src=\"https://unpkg.com/@hpcc-js/wasm@0.3.11/dist/index.min.js\"></script>"
      ++ "<script src=\"https://unpkg.com/d3-graphviz@3.0.5/build/d3-graphviz.js\"></script>"
      ++ "<div id=\"graph\" ></div>"
      ++ "<script>"
      ++ "var dot =\n"
      ++ "    [\n"
      ++ "        'digraph  {',\n"

-- | Html with script code for rendering the DOT digraph
postGraph :: Html
postGraph =
  preEscapedToHtml $
    "        '}'\n"
      ++ "     ];\n"
      ++ " \n"
      ++ " d3.select(\"#graph\").graphviz()\n"
      ++ "     .renderDot(dot.join(''));\n"
      ++ " \n"
      ++ " </script>\n"

renderMdToHtml :: Text -> Html
renderMdToHtml = preEscapedToHtml . commonmarkToHtml [] []

newPage :: Text
newPage =
     "Use WikiWords in PascalCase for Links. \n\n"
  <> "Use [Markdown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) to format page content"

mdMenu :: Text  -> Text
mdMenu pageName =
    "[home](/) | [site map](/actions/graph) |  [recent changes](/RecentChanges) | [find](/actions/find) | "
      <> ( if pageName == ""
             then "edit"
             else "[edit](/edit/" <> pageName <> ")"
         )
      <> " | &nbsp;&nbsp;&nbsp;&nbsp; built with [HsWiki](https://github.com/thma/HsWiki) \r\n\r\n"
