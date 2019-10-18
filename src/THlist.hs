{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

import Data.Kind (Constraint, Type)
import Control.Monad.Cont
import Data.Functor.Identity
import Data.Foldable
import Debug.Trace

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (t :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# ts) = t

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Show ts => Show (HList ts) where
  show HNil = ""
  show (t :# ts) = show t <> "," <> show ts

chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS cba f cb = cba g where
  g = \a -> f a $ cb

square :: Int -> Cont r Int
square n = return (n * n)

double :: Int -> Cont r Int
double n = return (n * 2)

mycallCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
mycallCC f = cont $ \h -> runCont (f $ \a -> cont $ \_ -> h a) h

run flag = do
  a <- cont $ \o -> o "hello" ++ o "world"
  b <- return "2"
  d <- callCC $ \exit -> if (a == "hello") then (exit "exit") else (return "noexit")
  return $ a ++ b  ++ d

calc n = do
  x <- trace "x" $ square n
  z <- cont $ \o -> trace "z" $ (o 1) + (o 1)
  return $ x +  z

callCCex4 :: Cont r Bool
callCCex4 = do
    val <- callCC $ \exit -> do
        innerval <- callCC $ \innerExit -> do
            exit True
            undefined
        undefined
    return val

forLoop :: Monad m => [a] -> (a -> ContT () m c) -> m ()
forLoop l f = runContT (traverse_ f l) return

breakout :: Monad m => ContT () m a
breakout = ContT $ \_ -> return ()

test :: ContT () IO ()
test = forLoop [1..7] $ \i -> do
  if (i == 4) then
    breakout
  else
    liftIO $ print i
