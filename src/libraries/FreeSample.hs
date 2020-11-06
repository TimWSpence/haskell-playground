{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Libraries.FreeSample where

import Control.Monad.Free
import Data.Functor
import System.Random

data Console a = PrintLine String a | ReadLine (String -> a)
  deriving (Functor)

printLine :: (Functor f, Console <-< f) => String -> Free f ()
printLine s = inject $ PrintLine s ()

readLine :: (Functor f, Console <-< f) => Free f String
readLine = inject $ ReadLine id

data RandomName a = RandomName (String -> a) deriving (Functor)

randomName :: (Functor f, RandomName <-< f) => Free f String
randomName = inject $ RandomName id

prog :: (Functor f, Console <-< f, RandomName <-< f) => Free f ()
prog = do
  printLine "Hello. What's your name?"
  name <- readLine
  rand <- randomName
  printLine $ "Hello " <> name <> ". I will call you " <> rand

interpConsole :: Console a -> IO a
interpConsole (PrintLine s next) = putStrLn s $> next
interpConsole (ReadLine cont) = cont <$> getLine

interpRandom :: RandomName a -> IO a
interpRandom (RandomName cont) =
  cont <$> do
    idx <- randomRIO (0, 2)
    return $ ["Steve", "Larry", "Bob"] !! idx

run :: IO ()
run = foldFree (interpConsole ||| interpRandom) prog

data (:-:) (f :: * -> *) (g :: * -> *) a = InL (f a) | InR (g a) deriving (Functor)

infixr 9 :-:

(|||) :: (forall a. f a -> h a) -> (forall a. g a -> h a) -> (forall a. (f :-: g) a -> h a)
(|||) f _ (InL fa) = f fa
(|||) _ g (InR ga) = g ga

class (f :: * -> *) <-< (g :: * -> *) where
  inj :: f a -> g a

instance f <-< f where
  inj = id

instance {-# OVERLAPPING #-} f <-< (f :-: g) where
  inj = InL

instance {-# OVERLAPPING #-} (f <-< h) => f <-< (g :-: h) where
  inj = InR . inj

inject :: (Functor f, Functor g, f <-< g) => f a -> Free g a
inject = liftF . inj
