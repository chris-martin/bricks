{-# LANGUAGE DeriveFunctor #-}

module ChrisMartinOrg.Post.Parse
    ( parsePost
    ) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Prelude

import ChrisMartinOrg.Content (parseContent)

import Control.Lens
import Data.Semigroup

import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as Text

data AccValidation e a = AccFailure e | AccSuccess a deriving Functor

instance Semigroup e => Applicative (AccValidation e) where
    AccFailure e1 <*> AccFailure e2 = AccFailure (e1 <> e2)
    AccFailure e1 <*> AccSuccess _  = AccFailure e1
    AccSuccess _  <*> AccFailure e2 = AccFailure e2
    AccSuccess f  <*> AccSuccess a  = AccSuccess (f a)

accValidationToEither :: AccValidation e a -> Either e a
accValidationToEither (AccFailure e) = Left e
accValidationToEither (AccSuccess a) = Right a

parsePost :: FilePath -- ^ The directory containing the post
          -> T.Text   -- ^ The content of the post.md file
          -> Either [Text] Post
parsePost dir text = accValidationToEither $ Post dir
    <$> getVal "title"
    <*> eitherVal chron
    <*> getVal "slug"
    <*> AccSuccess thumb
    <*> AccSuccess css
    <*> getVal "abstract"
    <*> AccSuccess redirectFrom
    <*> eitherVal body
  where
    (metaText, bodyText) = splitPost text

    meta :: Map Text Text
    meta = Map.fromList $ parseMeta metaText

    get :: Text -> Either Text Text
    get key = maybe (Left $ T.append "Missing: " key) Right $ getMaybe key

    getVal :: Text -> AccValidation [Text] Text
    getVal = eitherVal . get

    getMaybe :: Text -> Maybe Text
    getMaybe key = Map.lookup key meta

    chron :: Either Text Chron
    chron = do str <- T.unpack <$> get "date"
               left T.pack (parseChron str)

    css :: [Css]
    css = maybeToList $ (CssSource . (dir </>) . T.unpack) <$> getMaybe "css"

    thumb :: Maybe FilePath
    thumb = ((dir </>) . T.unpack) <$> getMaybe "thumbnail"

    redirectFrom :: [FilePath]
    redirectFrom = maybe [] (fmap T.unpack . T.splitOn "\n") $
        getMaybe "redirect from"

    body :: Either Text Content
    body = left T.pack (parseContent bodyText)

eitherVal :: Either a b -> AccValidation [a] b
eitherVal (Left  x) = AccFailure [x]
eitherVal (Right x) = AccSuccess  x

-- | Splits a post file in two parts: its metadata, and its body. The first line in
-- the file is discarded and used as the delimiter between the metadata and the body.
-- In other words, the metadata must be surrounded by a pair of identical lines.
--
-- >>> splitPost "---\nabc\ndef\n---\nghi\njkl"
-- ("abc\ndef","ghi\njkl")
splitPost :: T.Text -> (T.Text, T.Text)
splitPost text = splitOn2T sep otherLines where
    (firstLine, otherLines) = splitOn2T "\n" text
    sep = T.concat ["\n", firstLine, "\n"]

-- |
-- >>> parseMeta "abc: def"
-- [("abc","def")]
--
-- >>> :{
--   parseMeta $ Text.unlines [ "one:two"
--                            , "three: four"
--                            , "five:  six"
--                            , "       seven" ]
-- :}
-- [("one","two"),("three","four"),("five","six\nseven")]
parseMeta :: T.Text -> [(T.Text, T.Text)]
parseMeta meta = parseMetaKV <$> lineGroups where
    lineGroups = groupByStart ((/= " ") . T.take 1) $ T.lines meta

-- |
-- >>> parseMetaKV ["abc: def"]
-- ("abc","def")
--
-- >>> :{
--   parseMetaKV [ "abc:  def"
--               , "       ghi" ]
-- :}
-- ("abc","def\n ghi")
parseMetaKV :: [T.Text] -> (T.Text, T.Text)
parseMetaKV lines@(firstLine:_) = (T.strip k, T.intercalate "\n" vLines) where
    (k, v1) = splitOn2T ":" firstLine
    startCol = T.length k + 1 + T.length (T.takeWhile (== ' ') v1)
    vLines = T.drop startCol <$> lines
parseMetaKV [] = ("", "")

-- |
-- >>> groupByStart (== '-') "one-two-three"
-- ["one","-two","-three"]
groupByStart :: (a -> Bool) -> [a] -> [[a]]
groupByStart isStart = foldr f [] where
    f x acc = case acc of
        [] -> [[x]]
        groups@((y:_):_) | isStart y -> [x] : groups
        (group:otherGroups) -> (x:group) : otherGroups

-- Like breakOn, but does not include the pattern in the second piece. Or like
-- splitOn, but only performing a single split rather than arbitrarily many.
--splitOn2L :: L.Text -> L.Text -> (L.Text, L.Text)
--splitOn2L pat src = case L.breakOn pat src of
--    (x, y) -> (x, L.drop (L.length pat) y)

-- Like breakOn, but does not include the pattern in the second piece. Or like
-- splitOn, but only performing a single split rather than arbitrarily many.
splitOn2T :: T.Text -> T.Text -> (T.Text, T.Text)
splitOn2T pat src = case T.breakOn pat src of
    (x, y) -> (x, T.drop (T.length pat) y)
