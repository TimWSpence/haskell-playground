import Control.Arrow

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
