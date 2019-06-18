{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree

newtype Fix f = Fix { unFix :: f (Fix f)}

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = unFix >>> fmap (cata f) >>> f

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana f = Fix <<< fmap (ana f) <<< f

type RAlgebra f a = f (Fix f, a) -> a

para :: Functor f => RAlgebra f a -> Fix f -> a
para f = unFix >>> fmap (id &&& para f) >>> f

type RCoalgebra f a = a -> f (Either (Fix f) a)

apo :: Functor f => RCoalgebra f a -> a -> Fix f
apo f = Fix <<< fmap (id ||| apo f) <<< f

type CVAlgebra f a = f (Cofree f a) -> a

hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = ana coalg >>> cata alg

-- histo :: Functor f => CVAlgebra f a -> Fix f -> a
-- histo f = inner >>> extract where -- _inner :: Fix f -> Cofree f a
--   inner fix = unFix >>> fmap inner >>> (f &&& id)  >>> mkCofree
--   mkCofree (a,b) = a :< b

data Nat a = Zero | Succ a deriving (Functor, Show, Eq)

data ListF a b = Nil | Cons a b deriving Functor

alg :: ListF Int Int -> Int
alg Nil = 1
alg (Cons a b) = a*b

coalg :: Int -> ListF Int Int
coalg 0 = Nil
coalg n = Cons n (n-1)


fac :: Int -> Int
fac = hylo alg coalg
