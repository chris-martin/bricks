{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Hash
  ( hash
  , writeHashBS
  , writeHashFile
  ) where

import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Semigroup ((<>))
import Data.Text (Text)
import System.FilePath.Posix (takeExtension, (<.>), (</>), FilePath)

import qualified Crypto.Hash as Hash
import qualified Crypto.Hash.Algorithms as HashAlg
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Directory as Dir

hash :: ByteString -> Text
hash =
  hashToText . Hash.hash

hashToText :: Hash.Digest HashAlg.SHA3_256 -> Text
hashToText =
  Text.decodeUtf8 . Base16.encode . BS.take 32 . ByteArray.convert

writeHashBS
  :: ByteString -- ^ File content to write
  -> Text       -- ^ File extension
  -> IO Text    -- ^ Path of the file that was written
writeHashBS bs ext =
  let url = "hash/" <> hash bs <> "." <> ext
  in  BS.writeFile (Text.unpack $ "out/" <> url) bs $> url

writeHashFile
  :: Text            -- ^ Path of the source file
  -> IO (Maybe Text) -- ^ Path of the file that was written,
                     --   if the source does exist.
writeHashFile file =
  do
    exists <- Dir.doesFileExist $ Text.unpack file
    if exists
        then do
            bs <- BS.readFile $ Text.unpack file
            Just <$> writeHashBS bs ext
        else pure Nothing
  where
    ext = Text.pack $ takeExtension $ Text.unpack file
