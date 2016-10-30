{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Content
    ( resolveContentAssets
    , parseContent
    ) where

import ChrisMartinOrg.Core

import ChrisMartinOrg.Content.Parse (parseContent)
import ChrisMartinOrg.Hash (writeHashFile)

import Control.Monad (when)

import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L

import Data.Foldable (toList)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)

import Text.Blaze.Html5 (Html, preEscapedToHtml, toHtml, (!))

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- todo - rename this
resolveContentAssets :: FilePath -> Content -> IO Text
resolveContentAssets path xs =
    T.concat <$> sequence (resolveContentPartAssets path <$> toList xs)

resolveContentPartAssets :: FilePath -> ContentPart -> IO Text
resolveContentPartAssets _ (ContentText x) = pure x
resolveContentPartAssets _ (ContentCode lang code) = pure "[ CODE ]" -- todo
resolveContentPartAssets path (ContentAsset x) =
    maybe T.empty (T.pack . ("../" <>)) <$> resolveAsset path x

resolveAsset :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveAsset path asset = do
    outPathMaybe <- writeHashFile fullPath
    when (isNothing outPathMaybe) putError
    return outPathMaybe
  where
    fullPath = path <> "/" <> asset
    putError = putStrLn $ "Missing asset: " <> fullPath
