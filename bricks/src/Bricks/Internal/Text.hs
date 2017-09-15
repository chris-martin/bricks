{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Internal.Text

  ( Text

  , all
  , concat
  , intercalate
  , null
  , pack
  , replace
  , replicate
  , singleton
  , unpack
  , unwords

  ) where

-- Text
import           Data.Text (Text, all, null, pack, replace, replicate,
                            singleton, unpack, unwords)
import qualified Data.Text as Text

-- Base
import Data.Foldable (Foldable, toList)
import Data.Function ((.))

concat :: Foldable f => f Text -> Text
concat =
  Text.concat . toList

intercalate :: Foldable f => Text -> f Text -> Text
intercalate x =
  Text.intercalate x . toList
