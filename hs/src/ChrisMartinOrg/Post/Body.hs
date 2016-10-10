module ChrisMartinOrg.Post.Body
    ( resolveContentAssets
    ) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Hash (writeHashFile)

import Control.Monad (when)

import           Data.Maybe     (isNothing)
import qualified Data.Text      as T

import           Text.Blaze.Html5            (Html, preEscapedToHtml, toHtml,
                                              (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

resolveContentAssets :: FilePath -> Content -> IO T.Text
resolveContentAssets _ (ContentText x) = pure x
resolveContentAssets path (ContentAsset x) =
    maybe T.empty (T.pack . ("../" ++)) <$> resolveAsset path x
resolveContentAssets path (ContentList xs) =
    T.concat <$> sequence (resolveContentAssets path <$> xs)

resolveAsset :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveAsset path asset = do
    outPathMaybe <- writeHashFile fullPath
    when (isNothing outPathMaybe) putError
    return outPathMaybe
  where
    fullPath = path ++ "/" ++ asset
    putError = putStrLn $ "Missing asset: " ++ fullPath
