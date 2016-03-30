{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Post
    ( Post(..)
    , getPosts
    , Chron(..)
    , formatChron
    ) where

import ChrisMartinOrg.Core

import Prelude hiding (lines)

import Control.Applicative (liftA2)
import Control.Arrow       (left)
import Control.Lens
import Control.Monad       (mfilter)

import           Data.List          (sort)
import           Data.List          (findIndex)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (catMaybes, isJust)
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as L
import qualified Data.Text.Lazy.IO  as LTextIO
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Format   as TimeF

import Data.Validation (AccValidation (..), _AccValidation, _Either)

import qualified System.Directory   as Dir
import qualified System.Posix.Files as Files
import           Text.Blaze.Html5   (Html, preEscapedToHtml)
import           Text.Read          (readMaybe)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as Text

data Post = Post
    { postTitle    :: Html
    , postChron    :: Chron
    , postSlug     :: T.Text
    , postAbstract :: Html
    , postBody     :: Html
    }

getPosts :: IO [Post]
getPosts = do
    paths <- (reverse . sort) <$> listDirectory "in/posts"
    fmap catMaybes $ sequence $ getPost <$> paths

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
    (Post <$> val (preEscapedToHtml <$> getMeta "title")
          <*> val (do str <- T.unpack <$> getMeta "date"
                      left T.pack (parseChron str))
          <*> val (getMeta "slug")
          <*> val ((markdown . L.fromStrict) <$> getMeta "abstract")
          <*> (AccSuccess $ markdown body)
    )^._Either
  where
    metaDelim = head $ L.lines text
    (metaText, body) = splitOn2L (L.concat ["\n", metaDelim, "\n"]) (L.dropWhile (/= '\n') text)
    meta = Map.fromList $ parseMeta $ L.toStrict metaText
    getMeta key = maybe (Left $ T.append "Missing: " key) Right $ Map.lookup key meta
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

data Chron = Chron
    { chronYear  :: Int
    , chronMonth :: Int
    , chronDay   :: Maybe Int
    }

formatChron :: Chron -> T.Text
formatChron c = T.unwords $ concat [[ yearW, monthW ], maybe [] pure dayWMaybe]
  where yearW = T.pack $ show $ chronYear c
        monthW = T.pack $ fst $ TimeF.months TimeF.defaultTimeLocale !! ((chronMonth c) - 1)
        dayWMaybe = (T.pack . show) <$> chronDay c

parseChron :: String -> Either String Chron
parseChron t = do
    (yearW, monthW, dayWMaybe) <- case words t of
        [a, b]    -> Right (a, b, Nothing)
        [a, b, c] -> Right (a, b, Just c)
        _ -> Left "Invalid date: must have 2 or 3 words"
    year  <- case readMaybe yearW of
        Just x -> Right x
        _ -> Left "Invalid year"
    month <- case parseMonth monthW of
        Just x -> Right x
        _ -> Left "Invalid month"
    dayMaybe <- case dayWMaybe of
        Nothing   -> Right Nothing
        Just dayW -> case parseDay year month dayW of
            Just day -> Right $ Just day
            _ -> Left "Invalid day"
    Right $ Chron year month dayMaybe
  where
    months = snd <$> TimeF.months TimeF.defaultTimeLocale
    parseMonth mW = (+ 1) <$> findIndex (== take 3 mW) months
    validYMD y m d = isJust $ Cal.fromGregorianValid (fromIntegral y) m d
    parseDay y m dayW = mfilter (validYMD y m) (readMaybe dayW)

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
