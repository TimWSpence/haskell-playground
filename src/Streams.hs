{-# LANGUAGE TypeApplications #-}

module Streams
  (
  )
where

import Conduit
import Crypto.Hash
import Data.ByteArray
import Data.ByteString
import Data.ByteString.Base16

sha256 :: FilePath -> IO ByteString
sha256 file =
  runConduitRes . fmap (encode . convert . hashFinalize) $
    sourceFile file .| foldlC hashUpdate ctx
  where
    ctx = hashInit @SHA256
