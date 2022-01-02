{-# LANGUAGE OverloadedStrings #-}

module PageName
  ( PageName (..),
    asString,
    asText,
    isWikiWord,
    wikiWordToMdLink,
  )
where

import           Data.Text             (Text)
import qualified Data.Text             as T (pack, unpack)
import           Data.Text.ICU         (Regex, find)
import           Data.Text.ICU.Replace
import           Yesod                 (PathPiece (fromPathPiece, toPathPiece))

newtype PageName = Page String deriving (Eq, Read, Show)

instance PathPiece PageName where
  toPathPiece page = asText page
  fromPathPiece text = Just $ Page (T.unpack text)

asText :: PageName -> Text
asText (Page name) = T.pack name

asString :: PageName -> String
asString (Page name) = name

-- | the magic WikiWord Regex
wikiWordMatch :: Regex
wikiWordMatch = "(?<![\\(\\[])(([A-Z][a-z0-9]+){2,})(?![\\)\\]])"

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
