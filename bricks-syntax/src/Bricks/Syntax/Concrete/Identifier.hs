{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Syntax.Concrete.Identifier
  (
  -- * Type
    Identifier

  -- * Construction
  , identifier'try, identifier'orThrow

  -- * Deconstruction
  , identifier'text

  -- * Predicate
  , isValidIdentifier

  ) where

-- Bricks
import Bricks.Syntax.Concrete.Keyword

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Base
import qualified Data.Char as Char
import qualified Data.List as List
import           Prelude   (error)

{- $setup

>>> :set -XOverloadedStrings

-}

{- |

==== Construction

  - 'identifier'try'
  - 'identifier'orThrow'

==== Deconstruction

  - 'identifier'text'

==== See also

  - 'isValidIdentifier'

-}

newtype Identifier = Identifier Text
  deriving Show

identifier'text :: Identifier -> Text
identifier'text (Identifier x) = x

identifier'try :: Text -> Maybe Identifier
identifier'try x =
  if isValidIdentifier x then Just (Identifier x) else Nothing

identifier'orThrow :: Text -> Identifier
identifier'orThrow x =
  if isValidIdentifier x then Identifier x else
  error $ "String " <> show x <> " is not a valid identifier"

{- | Whether a string is a valid 'Identifier'.

==== Requirements for identifiers

We allow a string to render unquoted if all these conditions are met:

  - The first character is a letter or @_@
  - Every subsequent character is a letter, a number, or one of @+-*/_@
  - The string is not a keyword

-}

-- | ==== Examples
--
-- >>> isValidIdentifier "x"
-- True
--
-- An identifier cannot contain @'@.
--
-- >>> isValidIdentifier "x'"
-- False
--
-- An identifier cannot start with @_@, but not @-@.
--
-- >>> isValidIdentifier "_ab-c"
-- True
--
-- >>> isValidIdentifier "-ab-c"
-- False
--
-- The empty string is not a valid identifier.
--
-- >>> isValidIdentifier ""
-- False
--
-- An identifier can contain a number, but it cannot /start with/ a number.
--
-- >>> isValidIdentifier "a1"
-- True
--
-- >>> isValidIdentifier "1a"
-- False
--
-- A keyword is not a valid identifier.
--
-- >>> isValidIdentifier "let"
-- False
--
-- >>> isValidIdentifier "letx"
-- True

isValidIdentifier :: Text -> Bool
isValidIdentifier x =
  case Text.uncons x of
    Nothing -> False
    Just (y, zs) ->
      (Char.isAlpha y || y == '_')
      && Text.all (\c -> Char.isAlphaNum c || List.elem c ("+-*/_")) zs
      && List.all ((/= x) . keyword'text) keywords
