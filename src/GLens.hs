{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module GLens
  (
  )
where

import Control.Lens
import Control.Monad.Reader
import Data.Generics.Product
import Data.Generics.Product.Fields
import Data.Generics.Sum
import GHC.Generics (Generic)

data Person
  = Person
      { name :: String,
        age :: Int
      }
  deriving (Show, Generic)

data Dog
  = Dog
      { name :: String,
        age :: Int
      }
  deriving (Show, Generic)

data Living
  = Person' {name :: String, age :: Int}
  | Dog' {name :: String, age :: Int}
  deriving (Show, Generic)

data Trainer
  = Trainer
      { name :: String,
        dog :: Dog
      }
  deriving (Show, Generic)

data Foo = Foo {str :: String, bar :: String} deriving (Show, Generic)

foo :: (MonadReader r m, HasField' "str" r String, HasField' "bar" r String) => m String
foo = do
  s <- view (field' @"str")
  t <- reader (^. field' @"bar")
  return $ s <> t
