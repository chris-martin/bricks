{-# LANGUAGE ScopedTypeVariables #-}

module ChrisMartinOrg
  ( main
  ) where

import ChrisMartinOrg.Hash (writeHashFile)
import ChrisMartinOrg.Content (parseContent, resolveContentAssets)
import ChrisMartinOrg.Core
import ChrisMartinOrg.Css (compileCssSource)
import ChrisMartinOrg.Post (getPosts, writePost, postUrl)
import ChrisMartinOrg.Redirect (redirectHtml)

import qualified ChrisMartinOrg.Home as Home

import Control.Monad (forM_)
import Data.Functor (($>))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Safe (lastMay)
import System.FilePath.Posix ((</>), FilePath, takeDirectory)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Dir

inDir :: FilePath
inDir = "in"

defaultPostCssPath :: FilePath
defaultPostCssPath = inDir </> "posts/post.scss"

homeDir :: FilePath
homeDir = inDir </> "home"

homeContentPath :: FilePath
homeContentPath = homeDir </> "content.md"

homeCssPath :: FilePath
homeCssPath = homeDir </> "home.scss"

outDir :: FilePath
outDir = "out"

hashDir :: FilePath
hashDir = outDir </> "hash"

main :: IO ()
main = do

  -- set up output directories
  forM_ [ outDir, hashDir ] $ Dir.createDirectoryIfMissing True

  homeCss :: Either String CompiledCss <- compileCssSource homeCssPath

  ifLeft homeCss (\err ->
      putStrLn $ "Failed to compile the home page CSS "
              <> defaultPostCssPath <> " - " <> err)

  let homeCssMaybe :: Maybe CompiledCss =
          either (const Nothing) Just homeCss

  defaultPostCss :: Either String CompiledCss <-
      compileCssSource defaultPostCssPath

  let defaultPostCssMaybe :: Maybe CompiledCss =
          either (const Nothing) Just defaultPostCss

  ifLeft defaultPostCss (\err ->
      putStrLn $ "Failed to compile the default post CSS "
              <> defaultPostCssPath <> " - " <> err)

  posts :: [Post] <- do
      ps <- getPosts
      sequence (patchPost defaultPostCssMaybe <$> ps)

  homeSrc :: Text <- TextIO.readFile homeContentPath

  homeContent <-
    case parseContent homeSrc of
      Left err -> do
        putStrLn $ homeContentPath <> ": " <> err
        return $ singlePartContent $ ContentText homeSrc
      Right homeContent ->
        return homeContent

  homeText <- resolveContentAssets homeDir (homeContent :: Content)

  let homeHtml = renderHtml $ Home.pageHtml homeText homeCssMaybe posts
  LBS.writeFile "out/index.html" homeHtml

  forM_ posts $ writePost

  forM_ posts $ \post ->
    forM_ (postRedirectFrom post) $ \redirectFrom ->
      writeRedirect redirectFrom (postUrl post)

getRedirectPath :: FilePath -> FilePath
getRedirectPath [] = []
getRedirectPath ('/':x) = getRedirectPath x
getRedirectPath x =
  outDir </> if lastMay x == Just '/' then x </> "index.html" else x

writeRedirect :: FilePath -> FilePath -> IO ()
writeRedirect redirectFrom redirectTo =
  do
    let path = getRedirectPath redirectFrom
    Dir.createDirectoryIfMissing True $ takeDirectory path
    LBS.writeFile path $ renderHtml $ redirectHtml $ '/':redirectTo

ifLeft :: Monad m => Either a b -> (a -> m c) -> m ()
ifLeft e f = either (\x -> f x $> ()) (const $ pure ()) e

patchPost
  :: Maybe CompiledCss -- ^ Default post css
  -> Post -> IO Post
patchPost defaultPostCssMaybe p =
  do
    let postCss' = postCss p <>
                   (CssCompiled <$> Foldable.toList defaultPostCssMaybe)
    postThumb' <- resolveThumbMaybe (postThumb p)
    postTwitterImage' <- resolveTwitterImageMaybe (postTwitterImage p)
    return p { postCss          = postCss'
             , postThumb        = postThumb'
             , postTwitterImage = postTwitterImage'}

resolveThumb :: FilePath -> IO (Either String FilePath)
resolveThumb path =
  do
    outPathMaybe <- writeHashFile path
    return $ case outPathMaybe of
      Nothing -> Left ("Missing thumbnail: " <> path)
      Just t -> Right t

resolveThumbMaybe :: Maybe FilePath -> IO (Maybe FilePath)
resolveThumbMaybe Nothing = return Nothing
resolveThumbMaybe (Just t) =
  do
    e <- resolveThumb t
    case e of
      Left err -> putStrLn err $> Nothing
      Right t' -> return (Just t')

resolveTwitterImage :: FilePath -> IO (Either String FilePath)
resolveTwitterImage path = do
    outPathMaybe <- writeHashFile path
    return $ case outPathMaybe of
        Nothing -> Left ("Missing Twitter image: " <> path)
        Just t -> Right t

resolveTwitterImageMaybe :: Maybe FilePath -> IO (Maybe FilePath)
resolveTwitterImageMaybe Nothing = return Nothing
resolveTwitterImageMaybe (Just t) =
  do
    e <- resolveTwitterImage t
    case e of
      Left err -> putStrLn err $> Nothing
      Right t' -> return (Just t')
