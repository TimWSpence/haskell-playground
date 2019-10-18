{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Foo
  ()
  where

foo :: (Traversable t, Applicative f) => t (f a, b) -> f (t (a, b))
foo = traverse $ \(fa, b) -> fmap (,b) fa

data Validated a b = Invalid a | Valid b deriving (Functor, Show)

instance Monoid a => Applicative (Validated a) where
  pure = Valid

  (Invalid a) <*> (Invalid a') = Invalid $ a <> a'
  (Invalid a) <*> _ = Invalid a
  _ <*> Invalid a = Invalid a
  (Valid f) <*> (Valid x) = Valid $ f x

data Pair a = Pair a a deriving (Functor, Show)

instance Applicative Pair where
  pure a = Pair a a

  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

newtype Fn a b = Fn { runFn :: a -> b }

instance Functor (Fn a) where
  fmap f (Fn g) = Fn $ f . g

instance Applicative (Fn a) where
  pure = Fn . const

  (Fn f) <*> (Fn g) = Fn $ \a -> f a (g a)

instance Monad (Fn a) where
  return = pure

  (Fn f) >>= g = Fn $ \a -> runFn (g (f a)) a

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance Functor (ContT r m) where
  fmap f (ContT k) = ContT $ \c -> k (c . f)

instance Applicative (ContT r m) where
  pure a = ContT $ \f -> f a

  (ContT f) <*> (ContT x) = ContT $ \c -> f (\a -> x $ \b -> c (a b))

instance Monad (ContT r m) where
  return = pure

  (ContT k) >>= f = ContT $ \c -> k (\a -> runContT (f a) c)

