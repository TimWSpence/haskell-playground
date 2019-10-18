{-# LANGUAGE DeriveFunctor #-}

import Control.Comonad.Cofree
import Control.Comonad

data Expr a = ENum Int | EAdd a a deriving (Show, Eq, Functor)

data Nel a = Cons a (Nel a) deriving (Functor, Show)

takeNel :: Int -> Nel a -> Nel a
takeNel n (Cons a l) = Cons a (takeNel (n-1) l)

sumNel :: Num a => Nel a -> a
sumNel (Cons a l) = a + (sumNel l)

lengthNel :: Nel a -> Int
lengthNel (Cons a l) = 1 + (length l)

instance Comonad Nel where

  extract (Cons a t) = a

  duplicate c@(Cons a t) = Cons c (duplicate <$> t)

rollAvg :: Int -> Nel Int -> Nel Int
rollAvg window =  extend rolling where
  rolling l = let w = takeNel window l in sumNel w `div` lengthNel w
