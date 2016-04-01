{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Post
    ( Post(..)
    , getPosts
    , postUrl
    , pageHtml
    ) where

import ChrisMartinOrg.Chron
import ChrisMartinOrg.Core
import ChrisMartinOrg.Css   (styleLink)

import Prelude hiding (lines)

import Control.Applicative (liftA2)
import Control.Arrow       (left)
import Control.Lens

import           Data.List         (sort)
import qualified Data.Map.Strict   as Map
import           Data.Maybe        (catMaybes)
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as LTextIO

import Data.Validation (AccValidation (..), _AccValidation, _Either)

import qualified System.Directory as Dir

import           Text.Blaze.Html5            (Html, preEscapedToHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as Text

data Post = Post
    { postTitle    :: Html
    , postChron    :: Chron
    , postSlug     :: T.Text
    , postThumb    :: Maybe FilePath
    , postCss      :: Maybe FilePath
    , postAbstract :: T.Text
    , postBody     :: L.Text
    }

getPosts :: IO [(FilePath, Post)]
getPosts = do
    paths <- (reverse . sort) <$> listDirectory "in/posts"
    let xs = do path <- paths
                return $ (fmap . fmap) ((,) path) $ getPost path
    catMaybes <$> sequence xs

getPost :: FilePath -> IO (Maybe Post)
getPost path = do
    i <- Dir.doesFileExist fullPath
    if i
        then do
            text <- LTextIO.readFile fullPath
            case parsePost text of
                Left errs -> do
                    putStrLn path
                    sequence_ ((putStrLn . T.unpack . T.append "  ") <$> errs)
                    return Nothing
                Right post -> return $ Just post
        else pure Nothing
    where fullPath = "in/posts/" ++ path ++ "/post.md"

parsePost :: L.Text -> Either [T.Text] Post
parsePost text =
    (Post <$> val (preEscapedToHtml <$> get "title")
          <*> val (do str <- T.unpack <$> get "date"
                      left T.pack (parseChron str))
          <*> val (get "slug")
          <*> (AccSuccess $ T.unpack <$> getMaybe "thumbnail")
          <*> (AccSuccess $ T.unpack <$> getMaybe "css")
          <*> val (get "abstract")
          <*> (AccSuccess body)
    )^._Either
  where
    metaDelim = head $ L.lines text
    (metaText, body) = splitOn2L (L.concat ["\n", metaDelim, "\n"]) (L.dropWhile (/= '\n') text)
    meta = Map.fromList $ parseMeta $ L.toStrict metaText
    get key = maybe (Left $ T.append "Missing: " key) Right $ getMaybe key
    getMaybe key = Map.lookup key meta
    val = (^. _AccValidation) . (left pure)

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
parseMetaKV lines = (T.strip k, T.intercalate "\n" vLines) where
    (k, v1) = splitOn2T ":" $ head lines
    startCol = (T.length k) + 1 + (T.length $ T.takeWhile (== ' ') v1)
    vLines = T.drop startCol <$> lines

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
splitOn2L :: L.Text -> L.Text -> (L.Text, L.Text)
splitOn2L pat src = case L.breakOn pat src of
    (x, y) -> (x, L.drop (L.length pat) y)

splitOn2T :: T.Text -> T.Text -> (T.Text, T.Text)
splitOn2T pat src = case T.breakOn pat src of
    (x, y) -> (x, T.drop (T.length pat) y)

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
    filter (not . isSpecial) <$> Dir.getDirectoryContents path
    where isSpecial = liftA2 (||) (== ".") (== "..")

postUrl :: Post -> T.Text
postUrl p = T.concat
    [ T.pack $ show $ chronYear $ postChron p
    , "/"
    , postSlug p
    ]

pageHtml :: Post -> Maybe FilePath -> Html
pageHtml post stylePath = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title $ postTitle post
        mapM_ (styleLink . ("../" ++)) stylePath
    H.body $ do
        H.div ! A.class_ "container" $ do
            H.h1 $ postTitle post
            H.div ! A.class_ "post-body" $ markdown $ postBody post
