module ChrisMartinOrg.Content
    ( contentParser
    , parseContent
    ) where

import ChrisMartinOrg.Core

import Control.Applicative ((<|>), many)

import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L

contentParser :: A.Parser Content
contentParser = ContentList <$> many (asset <|> stuff)
    where
    asset :: A.Parser Content
    asset = (ContentAsset . T.unpack) <$> (open *> value <* close)
        where
        open = A.string (T.pack "${")
        value = A.takeWhile (/= '}')
        close = A.string (T.pack "}")
    stuff :: A.Parser Content
    stuff = ContentText <$> (A.string (T.pack "$") <|> A.takeWhile1 (/= '$'))

parseContent :: T.Text -> Either String Content
parseContent = A.eitherResult
             . A.parse contentParser
             . L.fromStrict
