{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Internal.Text

  ( Text

  , all
  , append
  , concat
  , concatMap
  , intercalate
  , intercalateMap
  , null
  , pack
  , replace
  , replicate
  , show
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
import Data.Functor (Functor, fmap)
import qualified Text.Show

concat :: Foldable f => f Text -> Text
concat =
  Text.concat . toList

concatMap :: Foldable f => (a -> Text) -> f a -> Text
concatMap f = foldr (append . f) Text.empty

intercalate :: Foldable f => Text -> f Text -> Text
intercalate x =
  Text.intercalate x . toList

intercalateMap :: (Foldable f, Functor f) => Text -> (a -> Text) -> f a -> Text
intercalateMap i f =
  intercalate i . fmap f

show :: Text.Show.Show a => a -> Text
show = Text.pack . Text.Show.show
