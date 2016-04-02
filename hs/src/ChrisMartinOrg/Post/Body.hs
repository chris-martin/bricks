module ChrisMartinOrg.Post.Body
    ( preprocessBody
    ) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Hash (writeHashFile)

import Control.Monad (when)

import Data.Maybe (isNothing)
import qualified Data.Text.Lazy       as L

import           Text.Blaze.Html5              (Html, preEscapedToHtml, toHtml,
                                                (!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

preprocessBody :: FilePath -> PostBody -> IO L.Text
preprocessBody _ (PostBodyText x) = pure x
preprocessBody path (PostBodyAsset x) =
    maybe L.empty L.pack <$> resolveAsset path x
preprocessBody path (PostBodyList xs) =
    L.concat <$> (sequence (preprocessBody path <$> xs))

resolveAsset :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveAsset path asset = do
    outPathMaybe <- writeHashFile $ fullPath
    when (isNothing outPathMaybe) putError
    return outPathMaybe
  where
    fullPath = "in/posts/" ++ path ++ "/" ++ asset
    putError = putStrLn $ "Missing asset: " ++ fullPath
