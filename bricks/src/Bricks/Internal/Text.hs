{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Internal.Text

  ( module Data.Text

  , concat
  , intercalate

  ) where

-- Text
import           Data.Text hiding (concat, intercalate)
import qualified Data.Text as Text

-- Base
import Data.Foldable (Foldable, toList)
import Data.Function ((.))

concat :: Foldable f => f Text -> Text
concat = Text.concat . toList

intercalate :: Foldable f => Text -> f Text -> Text
intercalate x = Text.intercalate x . toList
