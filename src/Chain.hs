{-# LANGUAGE DeriveFunctor #-}

module Chain
  (
  )
where

import Data.Foldable
import Data.Traversable
import Control.Applicative

data Chain a = CNil | CCons a (Chain a) | CAppend (Chain a) (Chain a) deriving Functor

instance Semigroup (Chain a) where
  (<>) = CAppend

instance Monoid (Chain a) where
  mempty = CNil

  mappend = (<>)

instance Foldable Chain where
  foldMap f CNil = mempty
  foldMap f (CCons a chain) = f a <> foldMap f chain
  foldMap f (CAppend l r) = foldMap f l <> foldMap f r

instance Traversable Chain where
  traverse f CNil = pure CNil
  traverse f (CCons a chain) = liftA2 CCons (f a) (traverse f chain)
  traverse f (CAppend l r) = liftA2 CAppend (traverse f l) (traverse f r)
