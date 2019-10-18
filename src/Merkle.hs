{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Merkle
  (
  -- ( Tree (..),
  --   merkleHash,
  )
where

-- import qualified Crypto.Hash.SHA256 as SHA256
-- import Data.Binary (encode)
-- import Data.ByteString
-- import qualified Data.ByteString.Lazy as DBL
-- import Data.Functor.Foldable
-- import Data.Functor.Foldable.TH
-- import Data.Hashable
-- import Data.Text
-- import Data.Text.Encoding

-- data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Functor)

-- makeBaseFunctor ''Tree

-- -- Compute the Merkle tree root of a tree
-- merkleHash :: Hashable a => Tree a -> ByteString
-- merkleHash = cata alg . hashLeaves
--   where
--     hashLeaves = fmap (DBL.toStrict . encode . hash)
--     alg :: TreeF ByteString ByteString -> ByteString
--     alg (LeafF h) = h
--     alg (NodeF h1 h2) = SHA256.hash $ h1 <> h2
