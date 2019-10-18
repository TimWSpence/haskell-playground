{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module DhallTest
  (
  )
where

import Codec.Serialise
import qualified Data.ByteString.Lazy as DBL
import Data.Void
import Dhall
import Dhall.Core
import Numeric.Natural
import Dhall.Binary

data Config
  = Config
      { port :: Natural,
        host :: String
      }
  deriving (Show, Generic)

deriving instance FromDhall Config

deriving instance ToDhall Config

loadConfig :: IO Config
loadConfig = inputFile auto "config.dhall"

encode :: Config -> DBL.ByteString
encode = serialise @(Expr Void Void) . denote . embed inject

encode2 :: Config -> DBL.ByteString
encode2 = encodeExpression . fmap absurd . denote . embed inject
