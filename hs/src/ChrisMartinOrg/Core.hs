{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Core
    ( markdown
    , hash
    , globalPageHeader
    , Page(..)
    ) where

import Data.Default

import qualified Crypto.Hash            as Hash
import qualified Crypto.Hash.Algorithms as HashAlg

import           Data.ByteString (ByteString)
import           Data.String     (IsString (..))
import qualified Data.Text.Lazy  as L

import           Text.Blaze.Html5            (Html, toHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Text.Markdown          as Markdown

markdown :: L.Text -> Html
markdown = Markdown.markdown def { Markdown.msXssProtect = False }

hash :: ByteString -> String
hash bs = take 32 $ show ((Hash.hash bs) :: Hash.Digest HashAlg.SHA3_256)

data Page = HomePage | PostPage

globalPageHeader :: Page -> Html
globalPageHeader page =
    H.header ! A.class_ "global-page-header" $
        H.div ! A.class_ "container" $
            case page of
                HomePage -> mempty
                _ -> H.a ! A.href ".." $ toHtml ("Chris Martin" :: String)
