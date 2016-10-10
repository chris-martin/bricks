{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChrisMartinOrg.Post
    ( getPosts
    , postUrl
    , writePost
    ) where

import           ChrisMartinOrg.Core
import           ChrisMartinOrg.Css        (compileCssFallback)
import           ChrisMartinOrg.Content    (resolveContentAssets)
import qualified ChrisMartinOrg.Post.Page  as Page
import           ChrisMartinOrg.Post.Parse (parsePost)

import Prelude hiding (lines)

import Control.Applicative (liftA2)
import Control.Exception   (try, IOException)
import Data.Functor (($>))

import qualified Data.ByteString.Lazy as LBS
import           Data.List            (sort)
import           Data.Maybe           (catMaybes)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import qualified Data.Text.IO         as TextIO

import qualified System.Directory      as Dir
import           System.FilePath.Posix (dropFileName, (</>))

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5              (toHtml)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as Text

getPosts :: IO [Post]
getPosts = do
    paths <- (reverse . sort) <$> listDirectory basePath
    catMaybes <$> sequence ((getPost . (basePath </>)) <$> paths)
  where
    basePath = "in" </> "posts"

getPost :: FilePath  -- ^ The directory containing the post
        -> IO (Maybe Post)
getPost dir = do
    i <- Dir.doesFileExist file
    if i
        then do
            textEither <- try $ TextIO.readFile file
            case textEither of
                Left (e :: IOException) -> (putStrLn $ show e) $> Nothing
                Right text ->
                    case parsePost dir text of
                        Left errs -> do
                            putStrLn dir
                            sequence_ ((putStrLn . T.unpack . T.append "  ") <$> errs)
                            return Nothing
                        Right post -> return $ Just post
        else pure Nothing
    where file = dir </> "post.md"

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
    filter (not . isSpecial) <$> Dir.getDirectoryContents path
    where isSpecial = liftA2 (||) (== ".") (== "..")

postUrl :: Post -> FilePath
postUrl p = (show $ chronYear $ postChron p) </> (T.unpack $ postSlug p)

writePost :: Post -> IO ()
writePost post = do
    Dir.createDirectoryIfMissing True dir
    pageInput <- getPageInput post
    LBS.writeFile file $ renderHtml $ Page.html pageInput
  where
    file = "out" </> (postUrl post)
    dir = dropFileName file

getPageInput :: Post -> IO Page.Input
getPageInput post = do
    css <- compileCssFallback $ postCss post
    bodyText <- resolveContentAssets (postDir post) (postBody post)
    return $ Page.Input
        { Page.inputTitle = toHtml $ L.fromStrict $ postTitle post
        , Page.inputChron = postChron post
        , Page.inputCss = css
        , Page.inputBody = markdown bodyText
        }
