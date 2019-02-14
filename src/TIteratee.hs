{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Exception
import Control.Monad as CM
import Control.Monad.Trans
import Data.Functor.Identity
import System.IO

data Stream el = EOF (Maybe String) | Chunk [el]

data Iteratee i m o = Done o
                      | Cont (Maybe String) (Stream i -> m (Iteratee i m o, Stream i))

type Enumerator i m o = Iteratee i m o -> m (Iteratee i m o)

type Enumeratee elo eli m a = Iteratee eli m a -> Iteratee elo m (Iteratee eli m a)

instance Monad m => Functor (Iteratee i m) where
  fmap = CM.liftM

instance Monad m => Applicative (Iteratee i m) where
  pure = return

  (<*>) = CM.ap

instance Monad m => Monad (Iteratee i m) where
  return = Done

  (Done i) >>= f = f i
  (Cont e k) >>= f = Cont e (k CM.>=> docase) where
    docase (Done o, s) = case f o of
      Cont Nothing k -> k s
      i            -> return (i, s)
    docase (i, s) = return (i >>= f, s)

instance MonadTrans (Iteratee i) where
  lift m = Cont Nothing (\s -> m >>= \x -> return (Done x, Chunk []))

run :: Monad m => Iteratee i m o -> m o
run (Done o) = return o
run (Cont Nothing k) = fst <$> k (EOF Nothing) >>= check where
  check (Done o) = return o
  check (Cont _ _) = error "Diverging iteratee"
run _ = error "Diverging iteratee"

fromStdIn :: Enumerator Char IO o
fromStdIn i@(Cont Nothing k) = do
  x <- getLine
  fst <$> k (Chunk x)
fromStdIn i = return i

fromList :: Monad m => [i] -> Enumerator i m o
fromList [] iter = return iter
fromList l@(h:t) iter = case iter of
  (Cont Nothing k) -> fst <$> k (Chunk l)
  _                -> return iter

ihead :: Monad m => Iteratee i m (Maybe i)
ihead = Cont Nothing k where
  k (Chunk []) = return (ihead, Chunk [])
  k (Chunk (h:t)) = return (Done (Just h), Chunk t)
  k s@(EOF _) = return (Done Nothing, s)

idrop :: Monad m => Int -> Iteratee i m ()
idrop 0 = Done ()
idrop n = Cont Nothing (step n) where
  step _ s@(EOF _) = return (Done (), s)
  step m s@(Chunk els) | length els <= m = return (idrop (m - length els), Chunk [])
  step m s@(Chunk els) = return (Done (), Chunk $ drop m els)

iprint :: Show i => Iteratee i IO ()
iprint = Cont Nothing k where
  k s@(EOF _) = return (Done (), s)
  k (Chunk els) = putStrLn (foldl (\acc e -> acc ++ "," ++ show e) "" els) >> return (Cont Nothing k, Chunk [])

icount :: Monad m => Iteratee i m Int
icount = Cont Nothing (k 0) where
  k n s@(EOF _) = return (Done n, s)
  k n (Chunk els) = return (Cont Nothing (k (n + length els)), Chunk [])

itoList :: Monad m => Iteratee i m [i]
itoList = Cont Nothing (k []) where
  k l (Chunk l2) = return (Cont Nothing (k (l ++ l2)), Chunk [])
  k l s@(EOF _) = return (Done l, s)

ifilter :: Monad m => (i -> Bool) -> Enumeratee i i m a
ifilter pred i@(Cont Nothing k) = Cont Nothing (f i) where
  f iter@(Cont Nothing k) s@(Chunk l) = k (Chunk $ filter pred l) >>= \(i, stream) -> return (Cont Nothing (f i), stream)
  f iter s = return (Done iter, s)
ifilter pred i = return i

itmap :: Monad m => (o -> i) -> Enumeratee o i m a
itmap f (Cont Nothing k) = Cont Nothing (step k) where
  step k (Chunk []) = return (Cont Nothing (step k), Chunk [])
  step k (Chunk l) = fst <$> k (Chunk $ map f l) >>= \i -> return (itmap f i, Chunk [])
  step k s = return (Done (Cont Nothing k), s)
itmap f i = return i

(>>>) :: Monad m => Enumerator i m o -> Enumerator i m o -> Enumerator i m o
e1 >>> e2 = e1 >=> e2

($=) :: Monad m => Enumeratee i o m a -> Iteratee o m a -> Iteratee i m a
enum $= iter = enum iter >>= lift . run
infixr 4 $=

foldFile :: (a -> Char -> a) -> a -> String -> IO a
foldFile f init path = withFile path ReadMode $ \handle ->
  loop handle init
  where
    loop h acc = do
      eof <- hIsEOF h
      if eof then return acc else do
        next <- hGetChar h
        loop h (f acc next)

