{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Content.Parse
    ( parseContent
    ) where

import ChrisMartinOrg.Core

import Control.Applicative
import Data.Attoparsec.Text (Parser, (<?>))
import Data.Semigroup
import Data.Text (Text)

import qualified Data.Attoparsec.Combinator as A (lookAhead)
import qualified Data.Attoparsec.Text as A
    ( choice, parseOnly
    , endOfInput, char, anyChar, manyTill, takeWhile
    , notInClass, takeWhile1 )
import qualified Data.Text as Text

---------------------------------------------------------------------

parseContent :: Text -> Either String Content
parseContent = A.parseOnly $ atNewBlock <* A.endOfInput


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
    newline = singlePartContent . ContentText . Text.singleton <$> A.char '\n'

stuff = asset <|> (singlePartContent . ContentText <$> text)
  where
    text = (A.takeWhile1 (A.notInClass "\n$")) <|>
           (Text.singleton <$> A.char '$')

-- | At least one newline, interpreted as regular text.
newlines = singlePartContent . ContentText <$> p <?> "newlines"
  where p = A.takeWhile1 (== '\n')

asset = singlePartContent . ContentAsset <$> p <?> "asset"
  where p = "${" *> A.manyTill A.anyChar (A.char '}')

end = mempty <$ A.endOfInput


---------------------------------------------------------------------
--  The code context
---------------------------------------------------------------------

code :: Parser Content
code = singlePartContent
    <$> liftA2 ContentCode codeOpen codeAtNewline
    <?> "code"

codeOpen :: Parser Text
codeOpen = "```" *> A.takeWhile (/= '\n') <* A.char '\n'

-- | In a code block: directly after a line break, or at the start
--   of the code body.
codeAtNewline :: Parser Text
codeAtNewline = end <|> (stuff <+> codeAtNewline)
  where
    end = "" <$ "```" <* A.lookAhead endOfBlock
    stuff = A.takeWhile (/= '\n') <+> "\n"


---------------------------------------------------------------------
--  Miscellaneous
---------------------------------------------------------------------

endOfBlock :: Parser ()
endOfBlock = A.choice
    [ () <$ "\n\n"
    , () <$ A.char '\n' <* A.endOfInput
    , () <$ A.endOfInput
    ]

(<+>) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<+>) = liftA2 (<>)
