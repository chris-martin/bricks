{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Internal.Text

  ( Text

  , all
  , append
  , concat
  , concatMap
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
import           Data.Text (Text, all, append, null, pack, replace, replicate,
                            singleton, unpack, unwords)
import qualified Data.Text as Text

-- Base
import Data.Foldable (Foldable, toList, foldr)
import Data.Function ((.))

concat :: Foldable f => f Text -> Text
concat =
  Text.concat . toList

concatMap :: Foldable f => (a -> Text) -> f a -> Text
concatMap f = foldr (append . f) Text.empty

intercalate :: Foldable f => Text -> f Text -> Text
intercalate x =
  Text.intercalate x . toList
