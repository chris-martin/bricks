{-# LANGUAGE OverloadedStrings #-}

module Bricks.Test.Hedgehog
  ( runTests
  ) where

import Control.Monad (unless)
import Data.Foldable (for_)

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
