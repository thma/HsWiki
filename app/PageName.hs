{-# LANGUAGE OverloadedStrings #-}

module PageName
  ( PageName,
    asString,
    asText,
    pageName,
    isWikiWord,
    wikiWordToMdLink,
    homePage,
    recentChanges
  )
where

import           Data.Text             (Text)
import qualified Data.Text             as T (pack, unpack)
import           Data.Text.ICU         (Regex, find, fromText)
import           Data.Text.ICU.Replace ( replaceAll )
import           Yesod                 (PathPiece (fromPathPiece, toPathPiece))

newtype PageName = Page Text deriving (Eq, Read, Show)

instance PathPiece PageName where
  toPathPiece page   = asText page
  fromPathPiece text = pageName text

asText :: PageName -> Text
asText (Page name) = name

asString :: PageName -> String
asString (Page name) = T.unpack name

pageName :: Text -> Maybe PageName
pageName name =
  if isWikiWord name
    then Just $ Page name
    else Nothing

-- | the magic WikiWord Regex
wikiWordMatch :: Regex
wikiWordMatch = "([A-Z][a-z0-9]+){2,}"
--wikiWordMatch = "(?<![\\(\\[])(([A-Z][a-z0-9]+){2,})(?![\\)\\]])"

-- | checks if a given Text is a WikiWord
isWikiWord :: Text -> Bool
isWikiWord pageName =
  case find wikiWordMatch pageName of
    Nothing -> False
    Just _  -> True

-- | converts a WikiWord into a Markdown link: [WikiWord](WikiWord)
wikiWordToMdLink :: Text -> Text
wikiWordToMdLink text =
  let match = wikiWordMatch
      replace = "[$0]($0)"
   in replaceAll match replace text

-- special page names are constructed here
homePage :: PageName
homePage = Page "HomePage"

recentChanges :: PageName
recentChanges = Page "RecentChanges"
