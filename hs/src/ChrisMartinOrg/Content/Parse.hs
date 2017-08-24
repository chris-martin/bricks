{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Content.Parse
  ( parseContent
  ) where

import ChrisMartinOrg.Core

import Control.Applicative ((<|>), liftA2)
import Data.Attoparsec.Text (Parser, (<?>))
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)

import qualified Data.Attoparsec.Combinator as A (lookAhead)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text

--------------------------------------------------------------------------------

parseContent :: Text -> Either String Content
parseContent = A.parseOnly $ atNewBlock <* A.endOfInput


--------------------------------------------------------------------------------
--  The default context
--------------------------------------------------------------------------------

-- | After at least two line breaks, or at the start of the document.
atNewBlock :: Parser Content
atNewBlock = (code <+> atNewBlock) <|> atNewLine

-- | Directly after one line break.
atNewLine :: Parser Content
atNewLine =
  (newlines <+> atNewBlock) <|>
  (stuff <+> atInline)      <|>
  end

-- | Not after a line break.
atInline :: Parser Content
atInline =
    (newline <+> atNewLine) <|>
    (stuff   <+> atInline)  <|>
    end
  where
    newline = singlePartContent . ContentText . Text.singleton <$> A.char '\n'

stuff :: Parser Content
stuff =
    asset <|> (singlePartContent . ContentText <$> text)
  where
    text = (A.takeWhile1 (A.notInClass "\n$")) <|>
           (Text.singleton <$> A.char '$')

-- | At least one newline, interpreted as regular text.
newlines :: Parser Content
newlines =
    singlePartContent . ContentText <$> p <?> "newlines"
  where
    p = A.takeWhile1 (== '\n')

asset :: Parser Content
asset =
    singlePartContent . ContentAsset <$> p <?> "asset"
  where
    p = "${" *> A.manyTill A.anyChar (A.char '}')

end :: Parser Content
end = mempty <$ A.endOfInput


--------------------------------------------------------------------------------
--  The code context
--------------------------------------------------------------------------------

code :: Parser Content
code =
    fmap singlePartContent p <?> "code"
  where
    p = ContentCode <$> codeOpen <*> codeAtNewline

codeOpen :: Parser Text
codeOpen =
    "```" *> A.takeWhile (/= '\n') <* A.char '\n'

-- | In a code block: directly after a line break, or at the start
--   of the code body.
codeAtNewline :: Parser Text
codeAtNewline =
    end <|> (stuff <+> codeAtNewline)
  where
    end = "" <$ "```" <* A.lookAhead endOfBlock
    stuff = A.takeWhile (/= '\n') <+> "\n"


--------------------------------------------------------------------------------
--  Miscellaneous
--------------------------------------------------------------------------------

endOfBlock :: Parser ()
endOfBlock =
  A.choice
    [ () <$ "\n\n"
    , () <$ A.char '\n' <* A.endOfInput
    , () <$ A.endOfInput
    ]

(<+>) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<+>) = liftA2 (<>)
