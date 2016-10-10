{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module ChrisMartinOrg.Core
    (
    -- * Types
      Page(..)
    , Css(..)
    , CompiledCss(..)
    , Post(..)
    , Content(..)

    -- * Functions
    , markdown
    , globalPageHeader

    , module ChrisMartinOrg.Core.Chron

    ) where

import ChrisMartinOrg.Core.Chron

import Data.Default
import Data.Maybe   (catMaybes)

import           Data.ByteString (ByteString)
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as L

import           Text.Blaze.Html5            (Html, toHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Text.Markdown as Markdown


-----------------------------------------------------------------
--  Types
-----------------------------------------------------------------

newtype CompiledCss = CompiledCss { compiledCssPath :: FilePath }

data Css = CssCompiled CompiledCss
         | CssSource FilePath

data Page = HomePage | PostPage

data Content = ContentText T.Text
              | ContentAsset FilePath
              | ContentList [Content]

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
