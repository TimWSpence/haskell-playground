{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Streaming
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Monoid
import System.IO

-- Represents a stream of elements of type a with an effect type f
data Stream (m :: * -> *) a where
  Yield :: m a -> Stream m a -> Stream m a
  Await :: m x -> (x -> Stream m a) -> Stream m a
  Done :: m () -> Stream m a

instance Monad m => Functor (Stream m) where
  fmap = liftM

instance Monad m => Applicative (Stream m) where
  pure = return

  (<*>) = ap

instance Monad m => Monad (Stream m) where
  return a = Yield (return a) (Done (return ()))

  (Yield ma next) >>= f = Await ma ((<> (next >>= f)) . f)
  (Await ma cont) >>= f = Await ma (cont >=> f)
  (Done close) >>= f = Done close

instance Monad m => Semigroup (Stream m a) where
  (Yield ma next) <> s = Yield ma (next <> s)
  (Await mx cont) <> s = Await mx ((<> s) . cont)
  (Done close) <> s = case s of
    (Yield ma next) -> Yield (close >> ma) next
    (Await mx cont) -> Await (close >> mx) cont
    (Done close') -> Done (close >> close')

instance Monad m => Monoid (Stream m a) where
  mempty = Done (return ())
  mappend = (<>)

fromList :: Monad m => [a] -> Stream m a
fromList [] = Done (return ())
fromList (h : t) = Yield (return h) (fromList t)

fromStdIn :: Stream IO String
fromStdIn = Await getLine return

fromFile :: FilePath -> Stream IO String
fromFile file = Await handle cont
  where
    handle = (openFile file ReadMode) >>= handleStatus
    handleStatus h = fmap (h,) (hIsEOF h)
    cont (h, isClosed) = if (isClosed) then Done (hClose h) else Yield (hGetLine h) (Await (handleStatus h) cont)

-- This is very inefficient as it doesn't close streams early
takeS :: Int -> Stream m a -> Stream m a
takeS 0 s = case s of
  (Yield _ next) -> takeS 0 next
  s@(Await mx cont) -> Await mx (takeS 0 . cont)
  (Done close) -> Done close
takeS n s = case s of
  (Yield ma next) -> Yield ma (takeS (n -1) next)
  (Await mx cont) -> Await mx (takeS n . cont)
  (Done close) -> Done close

toFile :: FilePath -> Stream IO String -> IO ()
toFile file stream = withFile file WriteMode $ \h -> drain . mapF (hPutStrLn h) $ stream

toList :: Monad m => Stream m a -> m [a]
toList (Yield ma next) = do
  h <- ma
  t <- toList next
  return (h : t)
toList (Await ma cont) = do
  a <- ma
  toList (cont a)
toList (Done close) = close >> return []

fold :: (Monad m) => (b -> a -> b) -> b -> Stream m a -> m b
fold f b (Yield ma next) = do
  b' <- fmap (f b) ma
  fold f (b') next
fold f b (Await ma cont) = do
  a <- ma
  fold f b (cont a)
fold _ b (Done close) = close >> return b

foldMonoid :: (Monad m, Monoid a) => Stream m a -> m a
foldMonoid = fold (<>) mempty

-- Run a stream purely for its effects
drain :: Monad m => Stream m a -> m ()
drain (Yield ma next) = ma >> drain next
drain (Await ma cont) = ma >>= (drain . cont)
drain (Done close) = close

-- Map an effectful computation across a stream
mapF :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapF f (Yield ma next) = Yield (ma >>= f) (mapF f next)
mapF f (Await ma cont) = Await ma (mapF f . cont)
mapF f (Done close) = Done close

-- Print each line in /tmp/data in turn
example1 :: IO ()
example1 = drain . mapF putStrLn $ fromFile "/tmp/data"

-- Duplicate every element in the input list -> [1,1,2,2,3,3]
example2 :: [Int]
example2 = runIdentity . toList . (>>= \x -> return x <> return x) $ fromList [1, 2, 3]

-- Concatenate files to list
example3 :: IO [String]
example3 = toList $ (fromFile "/tmp/data") <> (fromFile "/tmp/data2")

-- Sum list
example4 :: Int
example4 = getSum . runIdentity . foldMonoid . fmap Sum $ fromList [1, 2, 3, 4, 5]

-- Copy file
example5 :: IO ()
example5 = toFile "/tmp/copy" $ fromFile "/tmp/data"

-- Take from list -> [1,2,3]
example6 :: [Int]
example6 = runIdentity . toList . takeS 3 $ fromList [1, 2, 3, 4, 5]
