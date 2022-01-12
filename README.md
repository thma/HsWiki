# Writing a Wiki Server with Yesod

<a href="https://github.com/thma/HsWiki/actions"><img src="https://github.com/thma/HsWiki/workflows/Haskell%20CI/badge.svg" alt="Actions Status" /></a> 
<a href="https://github.com/thma/HsWiki"><img src="https://thma.github.io/img/forkme.png" height="20"></a></p>

<!-- MATH JAX-->
<script type="text/javascript" src="https://thma.github.io/static/mathjax/tex-mml-chtml.js"></script>

## Abstract 
In this blog post I'm presenting an implementation of a Wiki System 
in the spirit of the legendary [C2-Wiki](http://wiki.c2.com/) - written in Haskell with the [Yesod](https://www.yesodweb.com/) framework.

There will also be some nice add-ons like a graphical reprentation of the page links.

## Introduction 

> The WikiWikiWeb is the first wiki, or user-editable website. It was launched on 25 March 1995 by its inventor, programmer Ward Cunningham, to accompany the Portland Pattern Repository website discussing software design patterns. 
> 
>[cited from Wikipedia](https://en.wikipedia.org/wiki/WikiWikiWeb)


The [WikiWikiWeb](http://wiki.c2.com/) was the earliest incarnation of a collaborative hypertext platform on the internet.
It started with a small set of features which proved to provide the essential tools required to create a large content 
base with a dense hyperlink structure. 
Editing and creating new pages was extremely simple which fostered free contributions and a high frequency of 
interactions between participants. 

The most prominent features are:

- A tiny markup language allows basic adjustments of typography and layout.
- All content is rendered as HTML and thus allow easy navigation with any web browser.
- An inplace editor allows adhoc creation and editing of pages. 
  On saving edited content, the page switches back to display mode, which renders the markup as HTML.
- WikiWords, that is Text in PascalCase or [Upper Camel Case](https://en.wikipedia.org/wiki/Camel_case) are interpreted as
  hyperlinks. If such a hyperlink does not link to an existing page, the editor is opened for creating a new page.
  This mechanism allows to create hyperlinked content in a very fast manner.
- Clicking on a Page Title will display a list of all references to the current page.
  This allows to identify related topics and also to organize semantic networks by creating category pages 
  that just keep links to all pages in the category [CategoryCategory](http://wiki.c2.com/?CategoryCategory)
- The RecentChanges page shows the latest creation and edits to pages and thus makes it easy to identify hot topics
- There is a full text search available.

In the following I'm going to explain how I implemented each of those features.

## A simple markup language: Just use Markdown

The original WikiWikiWeb markup language provided basic syntax for layouting text content. 
Modern markup languages like Markdown are a more convenient to use, provide much more features and are already widely used.
So I'm going to use Markdown instead of the original markup language.

## Rendering content as HTML

Yesod comes with a set of [templating mechanisms](https://www.yesodweb.com/book/shakespearean-templates) that ease the generation of HTML, CSS and Javascript for dynamic web content. The HTML templating is backed by the [Blaze Html generator](https://hackage.haskell.org/package/blaze-html). Thus Yesod is optimized to use [Blaze](https://hackage.haskell.org/package/blaze-html) for HTML content. If, for example, the Blaze `Html` data type is returned from route-handlers, Yesod  will automatically set the Content-Type to `text/html`.

So my basic idea is to use a Markdown renderer that can output Blaze `Html`-data and let Yesod do all the heavy lifting.

I'm using the [cmark-gfm](https://hackage.haskell.org/package/cmark-gfm) library to render (GitHub flavoured) Markdown content to HTML. 
In order to output `Html`-data, my `renderMdToHtml` function has to look like follows: 

```haskell
import           CMarkGFM        (commonmarkToHtml)
import           Data.Text       (Text)
import           Text.Blaze.Html (Html, preEscapedToHtml)

renderMdToHtml :: Text -> Html
renderMdToHtml = preEscapedToHtml . commonmarkToHtml [] []
```

## Inplace Content Editing

### Type safe page names

In order to work with the wiki page names in a type safe manner we first introduce a newtype `PageName`.
In order to make sure that only proper [WikiWords](https://en.wikipedia.org/w/index.php?title=WikiWord) can be used as page names I'm using a [smart constructor](https://wiki.haskell.org/Smart_constructors) `pageName` which only constructs a `PageName`instance if the intented page name matches the `wikiWordMatch` regular expression:

```haskell
newtype PageName = Page Text deriving (Eq, Read, Show)

pageName :: Text -> Maybe PageName
pageName name =
  if isWikiWord name
    then Just (Page name)
    else Nothing

-- | checks if a given Text is a WikiWord
isWikiWord :: Text -> Bool
isWikiWord pageName =
  case find wikiWordMatch pageName of
    Nothing -> False
    Just _  -> True

-- | the magic WikiWord Regex
wikiWordMatch :: Regex
wikiWordMatch = "([A-Z][a-z0-9]+){2,}"    
```

### The Yesod routes for the editor

The following `PathPiece` instance declaration is required to use the `PageName` as part of a Yesod route definition:

```haskell
instance PathPiece PageName where
  toPathPiece page   = asText page
  fromPathPiece text = pageName text

asText :: PageName -> Text
asText (Page name) = name
```

Again the usage of the `pageName` smart constructor ensures that only proper WikiWord pagenames are constructed.

Here comes the [Yesod route definition](https://www.yesodweb.com/book/basics#basics_routing) for displaying and editing of wiki pages:

```haskell
newtype HsWiki = HsWiki
  { contentDir :: String
  }

mkYesod "HsWiki" [parseRoutes|
/#PageName      PageR     GET             -- (1)
/edit/#PageName EditR     GET POST        -- (2)
|]
```

Definition (1) can be read as follows: for any `PageName` that is accessed via a HTTP GET a route PageR is defined, which (according to the rules of the Yesod routing DSL) requires us to implement a function with the following signature:

```haskell
getPageR :: PageName -> Handler Html
```

This function will have to lookup an existing page, render its Markdown content to Html and return it a `Handler Html` object. We'll have a look at this function shortly.


The definition (2) states that for any route /edit/`PageName` two functions must be defined, one for GET one for POST:

```haskell
getEditR  :: PageName -> Handler Html
postEditR :: PageName -> Handler Html
```
If you want to know how exactly handler function are invoked from the Yesod framework and how the route dispatching works, please have a look at the excellent [Yesod documentation](https://www.yesodweb.com/book/) which features a complete walkthrough with a HelloWorld application.

### Serving an editor

Now let's study the implementation of these two function step by step, first the GET handler:

```haskell
-- | handler for GET /edit/#PageName
getEditR :: PageName -> Handler Html
getEditR pageName = do
  path <- getDocumentRoot                    -- obtain path to document root 
  let fileName = fileNameFor path pageName   -- construct a file from the page name
  exists <- liftIO $ doesFileExist fileName  -- check whether file already exists
  markdown <-
    if exists
      then liftIO $ TIO.readFile fileName    -- if file exists, assign markdown with file content
      else return newPage                    -- else assign markdown with default content
  return $ buildEditorFor pageName markdown  -- return Html for an Editor page

-- | retrieve the name of the HsWiki {contentDir} attribute, defaults to 'content'
getDocumentRoot :: Handler String
getDocumentRoot = getsYesod contentDir  

-- | construct the proper file name for a PageName
fileNameFor :: FilePath -> PageName  -> FilePath
fileNameFor path pageName = path ++ "/" ++ asString pageName ++ ".md"

-- | create default content for a new page
newPage :: Text
newPage =
     "Use WikiWords in PascalCase for Links. \n\n"
  <> "Use [Markdown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) to format page content"
```

As we can see from the reading of markdown content from files, the idea is to just keep all pages as static content files in the filesystem. By default these files reside in the local folder *content* (this folder can be configured by a commandline argument).

Next we'll have a look at the `buildEditorFor` function that will generate the actual Html content of the editor page:


```haskell
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
  ```

The most important element here is the creation of an Html `<form ...>...</form> element.
The action for that form is just the same page but with a `POST`-method (we'll come to the respective handler function `postEditR` shortly).

Now imagine we point our browser to `http://localhost:3000/edit/BrandNewPage`. Yesod will do the routing to `getEditR (Page "BrandNewPage")` and the generated Html for editing a new page 'BrandNewPage' will be sent back to the browser. The page will look like this:

![The Editor for a new page](img/editor.png)

As we can see, I've applied some basic CSS styling [(using Milligram CSS)](https://milligram.io/). This is done in the `pageHeader` function.

### processing the posting of data

The editor has two buttons, *SAVE* and *CANCEL*. On cancel we just navigate back to the previous page in the browser history. On save the browser sends the form data via the `POST` method to the server. To handle this incoming POST-request we'll the `postEditR` handler function:

```haskell
postEditR :: PageName -> Handler Html
postEditR pageName = do
  path <- getDocumentRoot                    -- obtain path to document root
  let fileName = fileNameFor path pageName   -- construct a file from the page name
  maybeContent <- lookupPostParam "content"  -- retrieve POST data
  client <- remoteHost <$> waiRequest        -- retrieve info on remote client from request
  case maybeContent of
    Just content -> liftIO $ do
      TIO.writeFile fileName content         -- if content exists write it to disk
      writeLogEntry path pageName client     -- also write a log entry to file RecentChanges
    Nothing -> return ()                     -- no content: do nothing
  redirect $ PageR pageName                  -- redirect to GET Page route (display content)
  ```

So essentially we are just writing the markdown content into a file. After that we redirect to 
the `PageR` route. This will result in redirecting the browser to `http://localhost:3000/BrandNewPage`. As you can see in the following screen-shot the markdown content that was entered in the editor form is now rendered as HTML:

![render existing page](img/renderPage.png)

### rendering page content

As promised above we'll now have a closer look at the `getPageR` route handler function:

```haskell
-- | Handler for GET /#PageName
getPageR :: PageName -> Handler Html
getPageR pageName = do
  path <- getDocumentRoot                            -- obtain path to document root 
  maybeShowRefs <- lookupGetParam "showBackrefs"     -- check whether URL ends with '?showBackrefs'
  maybeBackrefs <- liftIO $                          -- if showBackrefs was set, Just [PageName] 
    computeMaybeBackrefs path pageName maybeShowRefs -- else Nothing
  let fileName = fileNameFor path pageName           -- compute proper filename from pageName
  exists <- liftIO $ doesFileExist fileName          -- check whether such a file exists
  if exists
    then do                                                                  
      content <- liftIO $ TIO.readFile fileName      -- file exists, read its content
      return $ buildViewFor 
        pageName content maybeBackrefs               -- build HTML for content and return it
    else do
      redirect $ EditR pageName                      -- file does not exist, redirect to EditR
```

Let's ignore the lines with `maybeShowRefs` and `maybeBackrefs` for a moment. We just assume that `maybeBackrefs == Nothing`. So we first check whether a file exists for the given `pageName`. If yes, the file-content is read and bound to `content`; next we build a HTML view for the page with `buildViewFor` and return it. If no file was found matching `pageName` we redirect directly to the `EditR`which will in turn open up an editor for an empty page as already shown in the previous section.

Let's have a closer look at `buildViewFor`. It will first evaluate the `maybeBackrefs` arguments. For the moment let's assume equals `Nothing`, so that `hasBackref` is bound to `True` and `backrefEntry` to `""`. 

Then the actual HTML for the page is constructed from a set of template functions: 
- `pageHeader` creates the HTML head with css definitions, 
- `menuBar` creates the menu line on top of the page, 
- `pageTitle` creates a headline from the `pageName`, 
- `backrefEntry` is just empty text in this scenario
- `renderMdToHtml (wikiWordToMdLink content)` first replaces all ocurrences of *WikiWords* with proper Markdown hyperlinks of the form `[WikiWord](WikiWord)` the result is then rendered to HTML (this is the single place where we convert from *WikiWords* to hyperlinks and thus make the Wiki magic happen...), 
- finally `pageFooter` closes all open html tags:

```haskell
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

-- | converts a WikiWord into a Markdown link: [WikiWord](WikiWord)
wikiWordToMdLink :: Text -> Text
wikiWordToMdLink text =
  let match = wikiWordMatch
      replace = "[$0]($0)"
   in replaceAll match replace text              
```

## Displaying back links (aka reverse index) for each page

Another important feature of the original WikiWiki was the seamless integration of back links: 

> If page A links to page B, then a 'back link' would be a link which goes from page B back to page A.
>
>On this wiki, the title of each page works as a back link. Clicking on the title of any page finds all the pages referring to that page. It works for any wiki page. E.g. to find all pages that link to this page, click the title at the top of this page.
>
> [quoted from the WikiWiki](http://wiki.c2.com/?BackLink)

This feature can best be demonstrated with an example. First we lookup up page `http://localhost:3000/CategoryMammal`, a page meant to represent the class of all mammaĺ animals:

![CategoryMammal](img/CategoryMammal.png)

The headline of this page is a hyperlink which references `http://localhost:3000/CategoryMammal?showBackrefs`. Following the link results in the following page:


![CategoryMammal](img/CategoryMammalWithBackLinks.png)

Now we see a bullet point list of all pages linking to *CategoryMammal* above the normal page content. Following one of these links, e.g. `http://localhost:3000/SpeciesCat`, leads to the following page:

![SpeciesCat](img/SpeciesCat.png)

At the bottom of this page we see the *WikiWord* CategoryMammal. This is interpreted as a link from *SpeciesCat* to *CategoryMammal*. And as a result the back-link display on page *CategoryMammal* contains a link to *SpeciesCat*.

Let's see how this works on the code level. In fact we already came across this mechanism but had skipped over it for the time being. Now it's time to revisit. We start with the `getPageR` function.

In our scenario a click on the link `http://localhost:3000/CategoryMammal?showBackrefs` leads to a call to `getPageR`. But this time `lookupGetParam "showBackrefs"` will succeed and thus now `maybeShowRefs` is bound to `Just ""`. This will lead to a different execution path in the call to `computeMaybeBackrefs`:

```haskell
-- | Handler for GET /#PageName
getPageR :: PageName -> Handler Html
getPageR pageName = do
  path <- getDocumentRoot                            -- obtain path to document root 
  maybeShowRefs <- lookupGetParam "showBackrefs"     -- check whether URL ends with '?showBackrefs'
  maybeBackrefs <- liftIO $                          -- if showBackrefs was set, Just [PageName] 
    computeMaybeBackrefs path pageName maybeShowRefs -- else Nothing
  let fileName = fileNameFor path pageName           -- compute proper filename from pageName
  exists <- liftIO $ doesFileExist fileName          -- check whether such a file exists
  if exists
    then do                                                                  
      content <- liftIO $ TIO.readFile fileName      -- file exists, read its content
      return $ buildViewFor 
        pageName content maybeBackrefs               -- build HTML for content and return it
    else do
      redirect $ EditR pageName                      -- file does not exist, redirect to EditR

-- | if maybeShowRefs isJust then a list of a pages referencing pageName is computed
computeMaybeBackrefs :: FilePath -> PageName -> Maybe Text -> IO (Maybe [PageName])
computeMaybeBackrefs path pageName maybeShowRefs =
  case maybeShowRefs of
    Nothing -> return Nothing                      -- if maybeShowRefs == Nothing, return Nothing
    Just _  -> do                                  -- else compute list of all references to page by
      allPages <- computeIndex path                -- computing list of all pages in wiki
      backrefs <- computeBackRefs path pageName allPages -- compute all back references
      return $ Just backrefs                       -- return this list wrapped as a Maybe


-- | compute a list of all pages that contain references to pageName
computeBackRefs :: FilePath -> PageName -> [PageName] -> IO [PageName]
computeBackRefs path pageName allPages = do
  let filteredPages = delete pageName allPages   -- filter pagename from list of pages
  markRefs <- mapM                               -- create a list of bools: True if a page contains
    (fmap containsBackref . TIO.readFile . fileNameFor path)             -- a reference, else False
    filteredPages
  let pageBoolPairs = zip filteredPages markRefs -- create a zipped list of (pageName, Bool) pairs
  return $ map fst (filter snd pageBoolPairs)    -- return only pages marked True
  where
    containsBackref content =                    -- returns True if content contains pageName
      asText pageName `T.isInfixOf` content
```

Next we revisit `buildViewFor`. Here we see a case match on `maybeBackrefs`. In our current scenario it will
match to `Just backrefs`. Thus `renderedBackrefs` will be bound to Html generated by rendering a Markdown list of hyperlinks that is constructed from the `backrefs` list of PageNames. 

This generated Html is then included as `backrefEntry` into the overall page layout:

```haskell
buildViewFor :: PageName -> Text -> Maybe [PageName] -> Html
buildViewFor pageName content maybeBackrefs =
  let (hasBackref, backrefEntry) = case maybeBackrefs of
        Nothing       -> (False, text "")
        Just backrefs -> (True, renderedBackrefs)
          where
            concatMap :: (a -> Text) -> [a] -> Text
            concatMap = (T.intercalate "" .) . map
            renderedBackrefs = 
              renderMdToHtml $ concatMap 
                  ((\b -> "- [" <> b <> "](/" <> b <> ") \n") . asText) 
                  backrefs
   in toHtml [pageHeader False, 
              menuBar (asText pageName), 
              pageTitle pageName hasBackref, 
              backrefEntry, 
              renderMdToHtml (wikiWordToMdLink content), 
              pageFooter]        
```

## Show the latest creation and edits to pages 

I already covered the `postEditR` function, but I did not explain the `writeLogEntry` function which traces each change to page-content. So here comes the full picture:

```haskell
postEditR :: PageName -> Handler Html
postEditR pageName = do
  path <- getDocumentRoot                    -- obtain path to document root
  let fileName = fileNameFor path pageName   -- construct a file from the page name
  maybeContent <- lookupPostParam "content"  -- retrieve POST data
  client <- remoteHost <$> waiRequest        -- retrieve info on remote client from request
  case maybeContent of
    Just content -> liftIO $ do
      TIO.writeFile fileName content         -- if content exists write it to disk
      writeLogEntry path pageName client     -- also write a log entry to file RecentChanges
    Nothing -> return ()                     -- no content: do nothing
  redirect $ PageR pageName                  -- redirect to GET Page route (display content)
  
-- | write a log entry to the RecentChanges page
writeLogEntry :: FilePath -> PageName -> SockAddr -> IO ()
writeLogEntry path pageName client = do
  let fileName = fileNameFor path recentChanges -- path to RecentChanges page
  now <- getCurrentTime                         -- create timestamp
  let logEntry = toStrict $                     -- create a log entry consisting of:
        format ("- " % string % " " % string % " from " % string % "\n")
          (asString pageName)                   -- page edited/created
          (takeWhile (/= '.') (show now))       -- current timestamp
          (takeWhile (/= ':') (show client))    -- IP address of client
  TIO.appendFile fileName logEntry              -- add log entry at end of log file

-- | the RecentChanges PageName
recentChanges :: PageName
recentChanges = Page "RecentChanges"
```

And here comes a sample screen shot of the RecentChanges page:

![RecentChanges](img/RecentChanges.png)


## Have a full text search

For the full text search Iǜe provided a specific route `/actions/find` to avoid overlap with ordinary content pages:

```haskell
mkYesod "HsWiki" [parseRoutes|
/actions/find/  FindR     GET
|]
```

The corresponding handler function `getFindR` is defined as follows:

```haskell
-- | handler for GET /actions/find
getFindR :: Handler Html
getFindR = do
  path <- getDocumentRoot                       -- obtain path to document root
  allPages <- liftIO $ computeIndex path        -- compute a list of all page names in wiki
  maybeSearch <- lookupGetParam "search"        -- check whether query param 'search' is set
  case maybeSearch of
    Nothing     -> return $ buildFindPage "" [] -- if maybeSearch == Nothing or Just ""
    Just ""     -> return $ buildFindPage "" [] -- then return empty find page
    Just search -> do                           
      markMatches <- liftIO $                   -- else create a list of Bools by
        mapM                                    -- returning True for each file that matches
          (\p -> fmap containsSearchText $      -- search, else False
            return (asText p) <> TIO.readFile (fileNameFor path p)) 
          allPages
      let pageBoolPairs = zip allPages markMatches  -- create a zipped list [(PageName, Bool)]
      let matchingPages = map fst (filter snd pageBoolPairs) -- filter for all matching pages
      return $ buildFindPage search matchingPages   -- build find page with search term and 
        where                                       -- list of matching pages
          containsSearchText content = T.toLower search `T.isInfixOf` T.toLower content
```

The `buildFindPage` function is responsible for assembling the Html view of this page.


```haskell
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
```

The only interesting thing here is that I've include a bit of JavaScript to enable page updates while typing into the
find box. See the FindPage in action below:


![FindPage](img/FindPage.png)

## Provide a graph view of pages and their links

So far I've just reimplemented stuff that was already there in the original WikiWiki. While toying around with my HsWiki I thought it might be a nice addition to have a graph representation of the site content.

As always I try to code as little as possible myself and get the hard work done by the experts. In this case I'm relying on my alltime favourite Graph rendering library [GraphViz](https://graphviz.org/). This time in it's web assembly incarnation [d3-graphviz](https://github.com/magjac/d3-graphviz).

So again we'll have a specific route:

```haskell
mkYesod "HsWiki" [parseRoutes|
/actions/graph  GraphR    GET
|]
```

And a corresponding route handler function:

```haskell
-- | handler for GET /actions/graph
getGraphR :: Handler Html
getGraphR = do                                    
  path     <- getDocumentRoot                     -- obtain document root folder
  allPages <- liftIO $ computeIndex path          -- compute list of all wiki pages
  allRefs  <- liftIO $ mapM                       -- compute list of all back references
    (\p -> computeBackRefs path p allPages)       
    allPages                                      -- for each file in allPages
  return $ buildGraphView $ zip allRefs allPages  -- return Html view for [([PageName], PageName)] graph
```

Please note that this implementation has $O(n^2)$. This is caused by relying on `computeBackrefs` this function traverses all files and is called once for each file by `mapM`. 
Improving this is left as an exercise for the intereseted reader (all pull requests are welcome!)

The actual Html rendering is a bit more involved as I have to integrate the JS code for d3-graphviz and also to render the GraphViz DOT graph representation:

```haskell
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
```

You can see this in action in the following screen shot:

![SiteMap](img/SiteMap.png)

## Appendix

### How to build
    stack init
    stack install
    HsWiki

### Installation under Windows

Under Windows you will have to install the ICU library. I used the latest win64 version from https://github.com/unicode-org/icu/releases/tag/release-70-1. You'll have to manually copy *.ddl and *.h files to the following locations: 

- The actual lib files go to `C:\Users\<username>\AppData\Local\Programs\stack\x86_64-windows\msys2-<installdate>\mingw64\lib`
  Don't forget to strip version number from the .dll files (so icuuc70.dll becomes icuuc.dll)
- The include files go to `C:\Users\<username>\AppData\Local\Programs\stack\x86_64-windows\msys2-<installdate>\mingw64\include\unicode`
