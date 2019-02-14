{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverlappingInstances #-}

import Control.Monad.Free.Church
import Control.Monad.Free

data Console a = ReadLine (String -> a) | PrintLine String a deriving (Functor)

readLine :: (Functor f, Inj Console f) => Free f String
readLine = inject $ ReadLine id

printLine :: (Functor f, Inj Console f) => String -> Free f ()
printLine s = inject $ PrintLine s ()

data Foo a = Boom String a deriving (Functor)

boom :: (Functor f, Inj Foo f) => Free f ()
boom = inject $ Boom "boom" ()

interp :: Console a -> IO a
interp (ReadLine cont) = cont <$> getLine
interp (PrintLine str next) = putStrLn str >> return next

interpFoo :: Foo a -> IO a
interpFoo (Boom str next) = putStrLn "Boom" >> return next

prog :: Free (Console :+: Foo) ()
prog = do
  boom
  printLine "Hello, what is your name?"
  name <- readLine
  printLine $ "Hello " <> name

data (f :+: g) a = L (f a) | R (g a) deriving (Functor)

class (Functor f, Functor g) => Inj f g where
  inj :: f a -> g a

instance Functor f => Inj f f where
  inj = id

instance (Functor f, Functor g) => Inj f (f :+: g) where
  inj = L

instance (Functor f, Functor g, Functor h, Inj f h) => Inj f (g :+: h) where
  inj = R . inj

inject :: (Functor g, Inj f g) => f a -> Free g a
inject = liftF . inj

(.|.) :: (forall a. f a -> h a) -> (forall a. g a -> h a) -> (forall a. (f :+: g) a -> h a)
(k1 .|. k2) (L v) = k1 v
(k1 .|. k2) (R v) = k2 v
