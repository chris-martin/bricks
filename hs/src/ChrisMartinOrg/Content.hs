module ChrisMartinOrg.Content (contentParser) where

import ChrisMartinOrg.Core

import Control.Applicative ((<|>), many)

import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Text                 as T

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
