{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Libraries.GenericLensSample where

import Control.Lens
import Data.Function ((&))
import Data.Generics.Product.Fields
import Data.Generics.Product.Types
import GHC.Generics

data Pet = Dog String | Cat String deriving (Generic, Show)

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Generic, Show)

data Address = Address
  { street :: String,
    number :: Int,
    name :: Maybe String
  }
  deriving (Generic, Show)

data Family = Family {
  members :: [Person],
  address :: Address
                     } deriving (Generic, Show)

tim :: Person
tim = Person "tim" 31

laura :: Person
laura = Person "laura" 32

home :: Address
home = Address "some street" 4 Nothing

family  :: Family
family = Family [tim, laura] home

-- Access by field name
example1 = tim ^. field' @"name"

-- Modify by field name
example2 = tim & field' @"name" %~ (<> " spence")

-- Access by field type
example3 = tim ^. types @String

-- Access all names
example4 = family ^.. field' @"members" . traverse . field' @"name"

-- Modify all names
example5 = family & field' @"members" . traverse . field' @"name" %~ (<> " spence")

-- Access all strings recursively (names from people, street from address)
example6 = family ^.. types @String
