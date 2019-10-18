{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module KnownLengthList
  (
  )
where

import Data.Proxy
import GHC.TypeLits

data KnownLengthList (l :: Nat) a where
  Nill :: KnownLengthList 0 a
  (:::) :: a -> KnownLengthList l a -> KnownLengthList (l + 1) a

deriving instance Functor (KnownLengthList l)
deriving instance Show a => Show (KnownLengthList l a)

infixr 9 :::

klength :: forall l a. KnownNat l => KnownLengthList l a -> Integer
klength _ = natVal $ Proxy @l

toString :: Show a => KnownLengthList l a -> String
toString Nill = ""
toString (h ::: Nill) = show h
toString (h ::: t) = show h <> "," <> toString t

-- instance Show a => Show (KnownLengthList l a) where
--   show l = "[" <> toString l <> "]"
