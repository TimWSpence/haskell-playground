{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- https://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence/
module Futu where

import           Control.Monad.Free
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import qualified System.Random         as Random
import GHC.Generics

data Action = Flower | Grow | Branch

data Tree = Root Tree
          | Stalk Tree
          | Fork Tree Tree Tree
          | Bloom
          deriving (Show, Generic)

makeBaseFunctor ''Tree

-- type instance Base (Tree a) = Tree

data Seed = Seed { height :: Int, gen :: Random.StdGen } deriving (Show)

grow :: Seed -> (Action, Seed, Seed)
grow seed@Seed{..} = (choose choice, left {height = height + 1}, right { height = height + 1})
  where
    (choice, _) = Random.randomR(1 :: Int, 5) gen
    (leftR, rightR) = Random.split gen
    left = Seed height leftR
    right = Seed height rightR
    choose 1 = Flower
    choose 2 = Grow
    choose _ = Branch

sow :: Seed -> TreeF (Free TreeF Seed)
sow seed = case height seed of
  0 -> RootF (pure next1)
  10 -> BloomF
  _ -> case action of
    Flower -> BloomF
    Grow   -> StalkF (Free (sow next1))
    Branch -> ForkF (wrap (sow next1)) (wrap BloomF) (wrap (sow next2))
    Flower  -> BloomF
  where
    (action, next1, next2) = grow seed

run :: IO ()
run = do
  gen <- Random.newStdGen
  let tree = futu sow (Seed 0 gen) :: Tree
  print tree
