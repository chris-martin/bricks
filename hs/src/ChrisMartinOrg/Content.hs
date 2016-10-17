module ChrisMartinOrg.Content
    ( contentParser
    , parseContent
    , resolveContentAssets
    ) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Hash (writeHashFile)

import Control.Applicative ((<|>), many, liftA2)
import Control.Monad (when)

import qualified Data.Attoparsec.Text.Lazy as A
import           Data.Maybe     (isNothing)
import Data.Monoid ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L

import           Text.Blaze.Html5            (Html, preEscapedToHtml, toHtml,
                                              (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

contentParser :: A.Parser Content
contentParser = ContentList <$> many (asset <|> code <|> stuff)
  where
    asset :: A.Parser Content
    asset = (ContentAsset . T.unpack) <$> (open *> value <* close)
      where
        open = A.string (T.pack "${")
        value = A.takeWhile (/= '}')
        close = A.string (T.pack "}")

    code :: A.Parser Content
    code = (liftA2 ContentCode open body) <* close
      where
        open = A.string (T.pack "```") *> A.takeWhile1 (/= '\n')
               <* A.string (T.pack "\n")
        body = T.concat <$> takeT ()
        line = A.takeWhile1 (/= '\n') <> A.string (T.pack "\n")
        close = A.string (T.pack "```")

    stuff :: A.Parser Content
    stuff = ContentText <$> (A.string (T.pack "$"))
                        <|> (A.string (T.pack "`"))
                        <|> A.takeWhile1 (\c -> c /= '$' && c /= '`')

parseContent :: T.Text -> Either String Content
parseContent = A.eitherResult
             . A.parse contentParser
             . L.fromStrict

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
