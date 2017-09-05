{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module ChrisMartinOrg.Nix
  ( ReducedNixValue (..)
  , RenderNix (..)
  ) where

import Data.Map.Strict (Map)
import Data.Semigroup ((<>))
import Data.Text (Text)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text

-- | A fully reduced Nix expression.
data ReducedNixValue
  = NixNull
  | NixInt !Integer
  | NixBool !Bool
  | NixStr !Text
  | NixSet !(Map Text ReducedNixValue)

instance Show ReducedNixValue
  where
    show = Text.unpack . renderNix

class RenderNix a
  where
    renderNix :: a -> Text

instance RenderNix ReducedNixValue
  where
    renderNix =
      \case
        NixNull -> "null"
        NixInt i -> Text.pack (show i)
        NixBool True -> "true"
        NixBool False -> "false"
        NixStr s -> "\"" <> Text.replace "\"" "\\\"" s <> "\""
