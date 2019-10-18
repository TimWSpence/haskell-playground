{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Test where

import Control.Monad
import Control.Monad.ST

data Free (f :: * -> *) a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap = liftM

instance Functor f => Applicative (Free f) where
  pure = return

  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = Pure

  (Pure a) >>= f = f a
  (Free fFree) >>= f = Free $ fmap (>>= f) fFree


data HList (a :: [*]) where
  HNil :: HList '[]
  HCons :: a -> HList b -> HList (a ': b)

(<->) :: a -> HList b -> HList (a ': b)
(<->) = HCons

infixr 5 <->

len :: HList a -> Int
len HNil = 0
len (HCons a l) = 1 + len l

class HHead l where
  type Repr l
  hHead :: HList l -> Repr l


instance HHead (h ': t) where
  type Repr (h ': t) = h
  hHead (HCons h t) = h

class HAppend l1 l2 where
  hAppend :: HList l1 -> HList l2 -> HList (LConcat l1 l2)

type family LConcat (a :: [*]) (b :: [*]) where
  LConcat '[] l = l
  LConcat (h ': t) l = h ': (LConcat t l)

instance HAppend '[] l where
  hAppend HNil l = l


instance HAppend l1 l2 => HAppend (h ': l1) l2 where
  hAppend (HCons h t) l = HCons h (hAppend t l)

class ToString l where
  toString :: HList l -> String

instance {-# Overlapping #-} ToString '[] where
  toString HNil = ""

instance {-# Overlapping #-} Show a => ToString '[a] where
  toString (HCons a HNil) = show a

instance (Show a, ToString l) => ToString (a ': l) where

  toString (HCons a l) = (show a) <> "," <> (toString l)

instance ToString l => Show (HList l) where
  show l = "[" <> (toString l) <> "]"
