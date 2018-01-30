# HsWiki
Simple Wiki in the spirit of the legendary C2-Wiki - written in haskell with yesod

# Features
* Markup of wiki content is done with good old MarkDown.
* Automatic generation of new pages if non-existing local links are followed by the browser.
  (So to generate a new page just create a new link [new page](new_page) and click the new link.)


## How to build
    stack init
    stack install
    HsWiki

