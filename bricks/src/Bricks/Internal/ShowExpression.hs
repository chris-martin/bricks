{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Bricks.Internal.ShowExpression
  (
  -- * The 'ShowExpression' class
    ShowExpression (..)

  -- * Adapting 'ShowExpression' instances into 'Show' instances
  , showsPrec'showExpression

  -- * Functions for building 'ShowExpression' instances
  , showExpression'list
  , showExpression'list'text
  , showExpression'quoted'text
  , showExpression'paren

  ) where

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Base
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Prelude       (Int)

class ShowExpression a
  where
    showExpression :: a -> Text

showsPrec'showExpression :: ShowExpression a => Int -> a -> String -> String
showsPrec'showExpression _ x =
  (Text.unpack (showExpression x) <>)

showExpression'list :: (Foldable f, ShowExpression a) => f a -> Text
showExpression'list =
  showExpression'list'text . fmap showExpression . Foldable.toList

showExpression'list'text :: Foldable f => f Text -> Text
showExpression'list'text x =
  "[" <> Text.intercalate ", " x <> "]"

showExpression'quoted'text :: Text -> Text
showExpression'quoted'text =
  Text.pack . show @Text

showExpression'paren :: ShowExpression a => a -> Text
showExpression'paren x =
  "(" <> showExpression x <> ")"
