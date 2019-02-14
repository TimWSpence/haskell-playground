{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Exception
import Control.Monad as CM
import Data.Functor.Identity
import System.IO

data Free f a = Pure a | Suspend (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f free = case free of
    Pure a -> Pure (f a)
    Suspend fFree -> Suspend $ (fmap f) <$> fFree

instance Functor f => Applicative (Free f) where
  pure = return

  (<*>) = CM.ap

instance Functor f => Monad (Free f) where
  return = Pure

  free >>= f = case free of
    Pure a -> f a
    Suspend fFree -> Suspend $ (>>= f) <$> fFree

instance (Show a, Show (f (Free f a))) => Show (Free f a) where
        show (Pure x) = "(Pure (" ++ show x ++ "))"
        show (Suspend x) = "(Suspend (" ++ show x ++ "))"

liftF :: Functor f => f a -> Free f a
liftF fa = Suspend $ Pure <$> fa

infixr 0 ~>
type f ~> g = forall x. f x -> g x

freeM :: (Functor f, Functor g) => f ~> g -> Free f ~> Free g
freeM k (Pure a) = Pure a
freeM k (Suspend fFree) = Suspend . k $ freeM k <$> fFree

cata :: Monad m => Free m a -> m a
cata (Pure a) = return a
cata (Suspend mFree) = mFree >>= cata

data Alg a = GetLine (String -> a) | PrintLine String a

instance Functor Alg where
  fmap f (PrintLine s a) = PrintLine s (f a)
  fmap f (GetLine cont) = GetLine $ f . cont

get_line :: Free Alg String
get_line = liftF $ GetLine id

print_line :: String -> Free Alg ()
print_line s= liftF $ PrintLine s ()

prog :: Free Alg ()
prog = do
  input <- get_line
  print_line input

interp :: Alg ~> IO
interp (GetLine cont) = cont <$> getLine
interp (PrintLine s a) = putStrLn s >> return a
