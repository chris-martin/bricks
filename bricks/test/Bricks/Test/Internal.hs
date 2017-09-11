{-# LANGUAGE OverloadedStrings #-}

module Bricks.Test.Internal
  ( runTests
  , text
  ) where

import Control.Arrow ((>>>))
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Function (const)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.Text   as Text
import qualified Hedgehog
import qualified System.Exit as Exit
import qualified System.IO   as IO

runTests :: Hedgehog.Group -> IO ()
runTests tests =
  do
    for_ [IO.stdout, IO.stderr] $ \h -> do
      IO.hSetEncoding h IO.utf8
      IO.hSetBuffering h IO.LineBuffering
    success <- Hedgehog.checkParallel tests
    unless success Exit.exitFailure

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
