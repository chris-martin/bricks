{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Content.Parse
    ( parseContent
    ) where

import ChrisMartinOrg.Core

import Prelude hiding (concat, takeWhile)

import qualified Data.Sequence as Seq
import qualified Data.Text as Text

import Control.Applicative (many, liftA2, (<|>))
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Sequence (Seq)

import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
    ( (<?>), Parser, choice, eitherP, takeTill, parseOnly
    , endOfInput, char, anyChar, manyTill, takeWhile
    , notInClass, takeWhile1 )


---------------------------------------------------------------------

parseContent :: Text -> Either String Content
parseContent = parseOnly $ atNewBlock <* endOfInput


---------------------------------------------------------------------
--  The default context
---------------------------------------------------------------------

atNewBlock, atNewLine, atInline, newlines, stuff, asset, end :: Parser Content

-- | After at least two line breaks, or at the start of the document.
atNewBlock = (code <+> atNewBlock) <|> atNewLine

-- | Directly after one line break.
atNewLine = (newlines <+> atNewBlock) <|> (stuff <+> atInline) <|> end

-- | Not after a line break.
atInline =
    (newline <+> atNewLine) <|>
    (stuff   <+> atInline)  <|>
    end
  where
    newline = singlePartContent . ContentText . Text.singleton <$> char '\n'

stuff = asset <|> (singlePartContent . ContentText <$> text)
  where
    text = (takeWhile1 (notInClass "\n$")) <|>
           (Text.singleton <$> char '$')

-- | At least one newline, interpreted as regular text.
newlines = singlePartContent . ContentText <$> p <?> "newlines"
  where p = takeWhile1 (== '\n')

asset = singlePartContent . ContentAsset <$> p <?> "asset"
  where p = "${" *> manyTill anyChar (char '}')

end = mempty <$ endOfInput


---------------------------------------------------------------------
--  The code context
---------------------------------------------------------------------

code :: Parser Content
code = singlePartContent
    <$> liftA2 ContentCode codeOpen codeAtNewline
    <?> "code"

codeOpen :: Parser Text
codeOpen = "```" *> takeWhile (/= '\n') <* char '\n'

-- | In a code block: directly after a line break, or at the start
--   of the code body.
codeAtNewline :: Parser Text
codeAtNewline = end <|> (stuff <+> codeAtNewline)
  where
    end = "" <$ "```" <* lookAhead endOfBlock
    stuff = takeWhile (/= '\n') <+> "\n"

-- | In a code block: not after a line break.
codeAtInline :: Parser Text
codeAtInline = undefined


---------------------------------------------------------------------
--  Miscellaneous
---------------------------------------------------------------------

endOfBlock :: Parser ()
endOfBlock = choice
    [ () <$ "\n\n"
    , () <$ char '\n' <* endOfInput
    , () <$ endOfInput
    ]

(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 (<>)
