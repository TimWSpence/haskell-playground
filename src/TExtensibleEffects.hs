{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ExtensibleEffects(
                        ) where

import Control.Monad
import Unsafe.Coerce

data Eff r a where
  Pure :: a -> Eff r a
  Impure :: Union r b -> Arrs r b a -> Eff r a

type Arr r a b = a -> Eff r b
type Arrs r a b = FTCQueue (Eff r) a b

instance Functor (Eff r) where
  fmap = liftM

instance Applicative (Eff r) where
  pure = return

  (<*>) = ap

instance Monad (Eff r) where
  return = Pure

  (Pure a) >>= f = f a
  (Impure r k) >>= f = Impure r (Cons k f)

data FTCQueue m a b where
  Singleton :: (a -> m b) -> FTCQueue m a b
  Cons :: FTCQueue m a b -> (b -> m c) -> FTCQueue m a c
  Concat :: FTCQueue m a b -> FTCQueue m b c -> FTCQueue m a c

data View m a b where
  One :: (a -> m b) -> View m a b
  More :: (a -> m c) -> FTCQueue m c b -> View m a b

view :: FTCQueue m a b -> View m a b
view (Singleton f) = One f
view (Cons ftc f) = undefined
view (Concat ftc1 ftc2) = undefined

send :: (Member eff effs) => eff a -> Eff effs a
send eff = Impure (inj eff) (Singleton Pure)

------------------------------------------------- Open Union

data Union  (r :: [* -> *]) a where
  Union :: !Word -> t a -> Union r a

unsafeInj :: Word -> t a -> Union r a
unsafeInj = Union

unsafePrj :: Word -> Union r a -> Maybe (t a)
unsafePrj n (Union idx ta)
  | n == idx = Just $ unsafeCoerce ta
  | otherwise = Nothing

newtype P (t :: * -> *) (r :: [* -> *]) = P { unP :: Word }

class FindElem (t :: * -> *) (r :: [* -> *]) where
  elemIdx :: P t r

instance FindElem t (t ': r) where
  elemIdx = P 0

instance {-# OVERLAPPABLE #-}(FindElem t r) => FindElem t (t' ': r) where
  elemIdx = P $ 1 + unP (elemIdx :: P t r)

class FindElem eff effs => Member (eff :: * -> *)  effs where
  inj :: eff a -> Union effs a

  prj :: Union effs a -> Maybe (eff a)

instance FindElem eff effs => Member eff effs where
  inj = unsafeInj $ unP (elemIdx :: P eff effs)

  prj = unsafePrj $ unP (elemIdx :: P eff effs)