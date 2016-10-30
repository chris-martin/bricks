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

    , module ChrisMartinOrg.Core.Chron

    ) where

import ChrisMartinOrg.Core.Chron
import ChrisMartinOrg.Prelude

import Data.Default

import qualified Data.Sequence               as Seq
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Markdown               as Markdown

import Text.Blaze.Html5 (Html, toHtml, (!))


-----------------------------------------------------------------
--  Types
-----------------------------------------------------------------

newtype CompiledCss = CompiledCss { compiledCssPath :: FilePath }

data Css = CssCompiled CompiledCss
         | CssSource FilePath

data Page = HomePage | PostPage

newtype Content = Content { contentParts :: Seq ContentPart }

instance Monoid Content where
    mempty = Content mempty
    mappend (Content x) (Content y) = Content $ collapseSeqAppend f x y
      where
        f (ContentText x) (ContentText y) = Just $ ContentText (x <> y)
        f _ _ = Nothing

data ContentPart = ContentText T.Text
                 | ContentAsset FilePath
                 | ContentCode { codeLang :: T.Text
                               , codeBody :: T.Text }

data Post = Post
    { postDir      :: FilePath
    , postTitle    :: T.Text
    , postChron    :: Chron
    , postSlug     :: T.Text
    , postThumb    :: Maybe FilePath
    , postCss      :: [Css]
    , postAbstract :: T.Text
    , postBody     :: Content
    }


-----------------------------------------------------------------
--  Functions
-----------------------------------------------------------------

markdown :: T.Text -> Html
markdown = Markdown.markdown def { Markdown.msXssProtect = False } . L.fromStrict

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
