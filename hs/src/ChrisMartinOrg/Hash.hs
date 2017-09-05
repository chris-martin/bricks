module ChrisMartinOrg.Hash
  ( hash
  , writeHashBS
  , writeHashFile
  ) where

import Data.ByteString (ByteString)
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

writeHashBS :: ByteString -> String -> IO FilePath
writeHashBS bs ext =
  do
    BS.writeFile ("out/" <> url) bs
    return url
  where
    h = hash bs
    url = "hash" </> Text.unpack h <.> ext

writeHashFile :: FilePath -> IO (Maybe FilePath)
writeHashFile file =
  do
    exists <- Dir.doesFileExist file
    if exists
        then do
            bs <- BS.readFile file
            Just <$> writeHashBS bs ext
        else pure Nothing
  where
    ext = takeExtension file
