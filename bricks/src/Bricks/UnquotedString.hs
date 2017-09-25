{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.UnquotedString
  (
  -- * Type
    UnquotedString (..)

  -- * Constructor
  , unquotedString'try
  , unquotedString'orThrow

  -- * Predicates
  , text'canBeUnquoted
  , char'canBeUnquoted

  ) where

-- Bricks
import Bricks.Keyword

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Base
import qualified Data.Char as Char
import qualified Data.List as List
import           Prelude   (error)

{- | A string that can be rendered unquoted. Unquoted strings are restricted to
a conservative set of characters; see 'text'canBeUnquoted' for the full
rules.

The constructor is tagged "unsafe" because it lets you construct and invalid
value. Prefer 'unquotedString'try' which does validate the text.

This type does not represent a particular part of Brick syntax, but it is a
wrapper for 'Text' that enforces the limitations of strings at various places
in the Bricks syntax. -}
newtype UnquotedString = UnquotedString'Unsafe { unquotedString'text :: Text }

instance Show UnquotedString
  where
    showsPrec _ x = ("unquoted " <>) . shows (unquotedString'text x)

unquotedString'try :: Text -> Maybe UnquotedString
unquotedString'try x =
  if text'canBeUnquoted x then Just (UnquotedString'Unsafe x) else Nothing

-- | Throws an exception if the string cannot render unquoted.
unquotedString'orThrow :: Text -> UnquotedString
unquotedString'orThrow x =
  if text'canBeUnquoted x then UnquotedString'Unsafe x else
  error $ "String " <> show x <> " cannot render unquoted"

{- | Whether a string having this name can be rendered without quoting it.
We allow a string to render unquoted if all these conditions are met:

- The string is nonempty
- All characters satify 'char'canBeUnquoted'
- The string is not a keyword

>>> text'canBeUnquoted "-ab_c"
True

>>> text'canBeUnquoted ""
False

>>> text'canBeUnquoted "a\"b"
False

>>> text'canBeUnquoted "let"
False

-}
text'canBeUnquoted :: Text -> Bool
text'canBeUnquoted x =
  Text.all char'canBeUnquoted x
  && not (Text.null x)
  && List.all ((/= x) . keywordText) keywords

-- | Letters, @-@, and @_@.
char'canBeUnquoted :: Char -> Bool
char'canBeUnquoted c =
  Char.isLetter c || c == '-' || c == '_'
