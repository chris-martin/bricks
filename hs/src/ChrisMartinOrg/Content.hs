{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Content
    ( parseContent
    , resolveContentAssets
    , contentToHtml
    ) where

import ChrisMartinOrg.Core

import ChrisMartinOrg.Content.Parse (parseContent)
import ChrisMartinOrg.Hash (writeHashFile)

import qualified Data.Sequence        as Seq
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Highlighting.Kate      as Kate

import Control.Monad (when)
import Data.Foldable (fold, toList)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import Data.Text (Text)

import Text.Blaze.Html5 (Html, preEscapedToHtml, toHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)

resolveContentAssets :: FilePath -> Content -> IO Content
resolveContentAssets path xs =
    Content <$> sequence (resolveContentPartAssets path <$> contentParts xs)

resolveContentPartAssets :: FilePath -> ContentPart -> IO ContentPart
resolveContentPartAssets path (ContentAsset x) = do
    resolvedMaybe <- resolveAsset path x
    return $ case resolvedMaybe of
        Nothing -> ContentText ("${" <> T.pack x <> "}")
        Just resolved -> ContentAsset ("../" <> resolved)
resolveContentPartAssets _ x = pure x

resolveAsset :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveAsset path asset = do
    outPathMaybe <- writeHashFile fullPath
    when (isNothing outPathMaybe) putError
    return outPathMaybe
  where
    fullPath = path <> "/" <> asset
    putError = putStrLn $ "Missing asset: " <> fullPath

data C_Part = C_Text Text | C_Code Text Text

newtype C = C { c_parts :: Seq C_Part }

instance Monoid C where
    mempty = C mempty
    mappend (C x) (C y) = C $ collapseSeqAppend f x y
      where
        f (C_Text x) (C_Text y) = Just $ C_Text (x <> y)
        f _ _ = Nothing

contentPartToC :: ContentPart -> C_Part
contentPartToC (ContentText x) = C_Text x
contentPartToC (ContentAsset x) = C_Text $ T.pack x
contentPartToC (ContentCode lang body) = C_Code lang body

contentToHtml :: Content -> Html
contentToHtml = foldMap cpToHtml . c_parts . contentToC

contentToC :: Content -> C
contentToC = foldMap (C . Seq.singleton . contentPartToC) . contentParts

cpToHtml :: C_Part -> Html
cpToHtml (C_Text x) = H.div ! A.class_ "container" $ markdown x
cpToHtml (C_Code lang body) = H.toHtml
    $ Kate.formatHtmlBlock Kate.defaultFormatOpts
    $ Kate.highlightAs (T.unpack lang) (T.unpack body)
