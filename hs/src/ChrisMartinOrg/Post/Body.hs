module ChrisMartinOrg.Post.Body
    ( preprocessBody
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

preprocessBody :: FilePath -> PostBody -> IO T.Text
preprocessBody _ (PostBodyText x) = pure x
preprocessBody path (PostBodyAsset x) =
    maybe T.empty (T.pack . ("../" ++)) <$> resolveAsset path x
preprocessBody path (PostBodyList xs) =
    T.concat <$> sequence (preprocessBody path <$> xs)

resolveAsset :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveAsset path asset = do
    outPathMaybe <- writeHashFile fullPath
    when (isNothing outPathMaybe) putError
    return outPathMaybe
  where
    fullPath = path ++ "/" ++ asset
    putError = putStrLn $ "Missing asset: " ++ fullPath
