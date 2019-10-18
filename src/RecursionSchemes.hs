{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module RecursionSchemes where

import Control.Monad.Free
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Expr = Lit Int
          | Add Expr Expr
          | Mult Expr Expr

makeBaseFunctor ''Expr

-- data ExprF a
--  = Lit Int
--  | Add a a
--  | Mult a a
--  deriving (Show, Eq, Functor)

lit :: Int -> Fix ExprF
lit n = Fix $ LitF n

add :: Fix ExprF -> Fix ExprF -> Fix ExprF
add x y = Fix $ AddF x y

mult :: Fix ExprF -> Fix ExprF -> Fix ExprF
mult x y = Fix $ MultF x y

eval :: ExprF Int -> Int
eval (LitF n) = n
eval (AddF x y) = x + y
eval (MultF x y) = x * y

toString :: ExprF String -> String
toString (LitF n) = show n
toString (AddF x y) = "(" <> x <> " + " <> y <> ")"
toString (MultF x y) = "(" <> x <> " x " <> y <> ")"

-- expr :: Fix ExprF
-- expr = mult (lit 2) (add (lit 1) (lit 3))

expr :: Expr
expr = Mult (Lit 2) (Add (Lit 1) (Lit 3))

listAlg :: ListF Int Int -> Int
listAlg Nil = 0
listAlg (Cons n m) = n + m

printListAlg :: ListF Int ([Int], String) -> String
printListAlg Nil = ""
printListAlg (Cons n (l, r)) = case l of
  [] -> show n
  _ -> show n <> "," <> r
