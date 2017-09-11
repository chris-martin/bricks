{-# LANGUAGE OverloadedStrings #-}

module Bricks.Test.QQ
  ( text
  ) where

import Control.Arrow ((>>>))
import Data.Function (const)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.Text   as Text

text :: QuasiQuoter
text =
  QuasiQuoter
    { quoteExp  = pure . LitE . StringL . stripMargin
    , quotePat  = err
    , quoteType = err
    , quoteDec  = err
    }
  where
    err = const . fail $ "illegal text QuasiQuote (allowed as expression only)"

stripMargin :: String -> String
stripMargin =
  Text.pack
  >>> Text.splitOn "\n"
  >>> fmap (\x ->
        let (a, b) = Text.breakOn "|" x
        in  if Text.all (== ' ') a && not (Text.null b)
            then Text.drop 1 b
            else x)
  >>> Text.intercalate "\n"
  >>> Text.unpack
