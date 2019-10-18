{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Temp where

factors :: Int -> [Int]
factors 0 = []
factors 1 = []
factors 2 = [2]
factors n = reverse $ go n 2 []
  where
    go 1 _ ps = ps
    go n p ps | n `mod` p == 0 = go (n `quot` p) p (p : ps)
    go n p ps = go n (p + 1) ps

data OpenUnion r a where
  Union :: Word -> t a -> OpenUnion r a

class Member (t :: * -> *) (r :: [* -> *]) where
  indexOf :: Word

instance {-# OVERLAPPING #-} Member t r => Member t (s ': r) where
  indexOf = 1 + indexOf @t @r

instance {-# OVERLAPPING #-} Member t '[t] where
  indexOf = 0

data TQueue m a b where
  Leaf :: (a -> m b) -> TQueue m a b
  Node :: TQueue m a x -> TQueue m x b -> TQueue m a b

data Eff (r :: [*]) a where
  EPure :: a -> Eff r a
  EBind :: OpenUnion r b -> TQueue (Eff r) b a -> Eff r a
