{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Bricks.Identifiers
  ( isBareIdentifierName
  , isBareIdentifierChar
  ) where

import Bricks.Keywords
import Bricks.Types

import Data.Bool (Bool, (&&), (||), not)
import Data.Char (Char)
import Data.Eq ((==), (/=))
import Data.Function ((.))
import Data.Text (Text)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text

{- | Whether an identifier having this name can be rendered without quoting it.
We allow a name to be a bare identifier, and thus to render unquoted, if all
these conditions are met:

- The string is nonempty
- All characters satify 'isBareIdentifierChar'
- The string is not a keyword

>>> isBareIdentifierName "-ab_c"
True

>>> isBareIdentifierName ""
False

>>> isBareIdentifierName "a\"b"
False

>>> isBareIdentifierName "let"
False

-}
isBareIdentifierName :: Text -> Bool
isBareIdentifierName x =
  Text.all isBareIdentifierChar x
  && not (Text.null x)
  && List.all ((/= x) . keywordText) keywords

-- | Letters, @-@, and @_@.
isBareIdentifierChar :: Char -> Bool
isBareIdentifierChar c =
  Char.isLetter c || c == '-' || c == '_'
