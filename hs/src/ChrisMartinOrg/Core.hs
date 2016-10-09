{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module ChrisMartinOrg.Core
    (
    -- * Types
      Page(..)
    , Css(..)
    , CompiledCss(..)
    , Post(..)
    , PostBody(..)
    , Fallback(..)

    -- * Functions
    , markdown
    , globalPageHeader
    , firstJust

    , module ChrisMartinOrg.Core.Chron

    ) where

import ChrisMartinOrg.Core.Chron

import Data.Default
import Data.Maybe   (catMaybes)

import           Data.ByteString (ByteString)
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as L

import Safe (headMay)

import           Text.Blaze.Html5            (Html, toHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Text.Markdown as Markdown


-----------------------------------------------------------------
--  Types
-----------------------------------------------------------------

-- | Alternatives for a value that has fallbacks in case of failure.
type Fallback x = [x]

newtype CompiledCss = CompiledCss { compiledCssPath :: FilePath }

data Css = CssCompiled CompiledCss
         | CssSource FilePath

data Page = HomePage | PostPage

data PostBody = PostBodyText T.Text
              | PostBodyAsset FilePath
              | PostBodyList [PostBody]

data Post = Post
    { postDir      :: FilePath
    , postTitle    :: T.Text
    , postChron    :: Chron
    , postSlug     :: T.Text
    , postThumb    :: Maybe FilePath
    , postCss      :: Fallback Css
    , postAbstract :: T.Text
    , postBody     :: PostBody
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

firstJust :: [Maybe a] -> Maybe a
firstJust = headMay . catMaybes
