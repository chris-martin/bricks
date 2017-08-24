{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Core
  (
  -- * Types
    Page(..)
  , Css(..)
  , CompiledCss(..)
  , Post(..)
  , Content(..)
  , ContentPart(..)

  -- * Functions
  , markdown
  , globalPageHeader
  , singlePartContent
  , collapseSeqAppend

  ) where

import ChrisMartinOrg.PostDate

import Data.Semigroup (Semigroup ((<>)))
import Data.Sequence (Seq)
import Data.Text (Text)
import Text.Blaze.Html5 (Html, toHtml, (!))
import Text.Markdown (defaultMarkdownSettings)

import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as L
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Markdown as Markdown


-----------------------------------------------------------------
--  Types
-----------------------------------------------------------------

newtype CompiledCss = CompiledCss { compiledCssPath :: FilePath }
    deriving Show

data Css = CssCompiled CompiledCss
         | CssSource FilePath
    deriving Show

data Page = HomePage | PostPage
    deriving (Show, Eq)

newtype Content = Content { contentParts :: Seq ContentPart }

instance Semigroup Content where
    Content x <> Content y = Content $ collapseSeqAppend f x y
      where
        f (ContentText x) (ContentText y) = Just $ ContentText (x <> y)
        f _ _ = Nothing

instance Monoid Content where mempty = Content mempty; mappend = (<>)

data ContentPart = ContentText Text
                 | ContentAsset FilePath
                 | ContentCode { codeLang :: Text
                               , codeBody :: Text }

data Post =
  Post
    { postDir          :: FilePath
    , postTitle        :: Text
    , postDate         :: PostDate
    , postSlug         :: Text
    , postThumb        :: Maybe FilePath
    , postCss          :: [Css]
    , postAbstract     :: Text
    , postRedirectFrom :: [FilePath]
    , postBody         :: Content
    , postTwitterCard  :: Maybe Text
    , postTwitterImage :: Maybe FilePath
    , postTwitterDescription :: Maybe Text
    }


-----------------------------------------------------------------
--  Functions
-----------------------------------------------------------------

markdown :: Text -> Html
markdown =
  Markdown.markdown defaultMarkdownSettings { Markdown.msXssProtect = False } .
  L.fromStrict

globalPageHeader :: Page -> Html
globalPageHeader page =
    H.header ! A.class_ "global-page-header" $
        H.div ! A.class_ "container" $
            case page of
                HomePage -> mempty
                _ -> H.a ! A.href ".." $ toHtml ("Chris Martin" :: String)

singlePartContent :: ContentPart -> Content
singlePartContent = Content . Seq.singleton

collapseSeqAppend :: (a -> a -> Maybe a) -> Seq a -> Seq a -> Seq a
collapseSeqAppend f x y = maybe (x <> y) id $ do
    x' <- seqLast x
    y' <- seqFirst y
    collapsed <- f x' y'
    return $ Seq.take (length x - 1) x <>
             Seq.singleton collapsed   <>
             Seq.drop 1 y
  where
    seqIndexMaybe :: Seq a -> Int -> Maybe a
    seqIndexMaybe xs i =
        if null xs then Nothing else Just (Seq.index xs i)
    seqFirst xs = seqIndexMaybe xs 0
    seqLast xs = seqIndexMaybe xs (length xs - 1)
