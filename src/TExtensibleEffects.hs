{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module TExtensibleEffects where

import Control.Monad
import Unsafe.Coerce

data Eff r a where
  Pure :: a -> Eff r a
  Impure :: Union r b -> Arrs r b a -> Eff r a

type Arr r a b = a -> Eff r b
type Arrs r a b = FTCQueue (Eff r) a b

instance Functor (Eff r) where
  fmap = liftM

instance Applicative (Eff r) where
  pure = return

  (<*>) = ap

instance Monad (Eff r) where
  return = Pure

  (Pure a) >>= f = f a
  (Impure r k) >>= f = Impure r (Concat k (tsingleton f))

data FTCQueue m a b where
  Singleton :: (a -> m b) -> FTCQueue m a b
  Concat :: FTCQueue m a b -> FTCQueue m b c -> FTCQueue m a c

qApp :: Arrs r a b -> a -> Eff r b
qApp (Singleton f) a = f a
qApp q@(Concat q1 q2) a = qApp q1 a >>= qApp q2

qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
qComp arr h = h . qApp arr

send :: (Member eff effs) => eff a -> Eff effs a
send eff = Impure (inj eff) (tsingleton Pure)

handleRelay
  :: (a -> Eff r w)
  -- ^ handle pure value
  -> (forall v. t v -> Arr r v w -> Eff r w)
  -- ^ handle request for effect of type t
  -> Eff (t ': r) a
  -> Eff r w
handleRelay ret _ (Pure a) = ret a
handleRelay ret f i@(Impure u q) = case decomp u of
  Left ra -> Impure ra (tsingleton k)
  Right ta -> f ta k
  where
    k = qComp q (handleRelay ret f)

run :: Eff '[] a -> a
run (Pure a) = a
run _ = error "Run: should never happen"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure a) = return a
runM (Impure r arrs) = extract r >>= runM . qApp arrs

--------------------------------------------------- Builtins

data Reader r a where
  Ask :: Reader i i

ask :: Member (Reader i) r => Eff r i
ask = send $ Ask

runReader :: i -> Eff (Reader i ': r) a -> Eff r a
runReader i = handleRelay return (\Ask k -> k i)

data Writer w a where
  Tell :: Monoid w => w -> Writer w ()

tell :: (Monoid w, Member (Writer w) r) => w -> Eff r ()
tell w = send $ Tell w

runWriter :: Monoid w => w -> Eff (Writer w ': r) a -> Eff r (a, w)
runWriter w = handleRelay (\a -> return (a, mempty))
    (\(Tell w) k -> k () >>= (\(x, ws) -> return (x, w <> ws)))

data State s a where
  Get :: State s s
  Put :: s -> State s ()

get :: Member (State s) r => Eff r s
get = send $ Get

put :: Member (State s) r => s -> Eff r ()
put s = send $ Put s

modify :: Member (State s) r => (s -> s) -> Eff r ()
modify f = do
  current <- get
  let updated = f current
  put updated

runState :: s -> Eff (State s ': r) a -> Eff r a
runState s (Pure a) = return a
runState s (Impure u q) = case decomp u of
  Left u' -> Impure u' (tsingleton $ k s)
  Right st -> case st of
    Get -> runState s $ qApp q s
    Put s' -> runState s' $ qApp q ()
  where
    k s = qComp q (runState s)

prog :: (Member (Reader String) r, Member (Writer [String]) r, Member (State String) r) => Eff r String
prog = do
  r <- ask
  s <- get @String
  tell [s]
  put "baz"
  tell ["foo"]
  t <- get @String
  tell [t]
  tell [r]
  return r

app ::  Eff (Reader String ': Writer [String] ': State String ': '[]) a -> Eff (Reader String ': Writer [String] ': State String ': '[]) a
app = id

runProg :: (String, [String])
runProg = run . runState "state" . runWriter [] . runReader "foo" $ app prog

--------------------------------------------------- FTCQueue

tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton = Singleton

tsnoc :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
tsnoc ftc f = Concat ftc (tsingleton f)

tconcat :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
tconcat = Concat

data View m a b where
  One :: (a -> m b) -> View m a b
  More :: (a -> m c) -> FTCQueue m c b -> View m a b

view :: FTCQueue m a b -> View m a b
view (Singleton f) = One f
view (Concat ftc1 ftc2) = go ftc1 ftc2
  where
    go :: FTCQueue m a c -> FTCQueue m c b -> View m a b
    go (Singleton x) ftc = More x ftc
    go (Concat f1 f2) f3 = go f1 (Concat f2 f3)

------------------------------------------------- Open Union

data Union  (r :: [* -> *]) a where
  Union :: !Word -> t a -> Union r a

extract :: Union '[t] a -> t a
extract (Union _ ta) = unsafeCoerce ta

decomp :: Union (t ': r) a -> Either (Union r a) (t a)
decomp (Union 0 ta) = Right $ unsafeCoerce ta
decomp (Union n ra) = Left $ Union (n-1) ra


unsafeInj :: Word -> t a -> Union r a
unsafeInj = Union

unsafePrj :: Word -> Union r a -> Maybe (t a)
unsafePrj n (Union idx ta)
  | n == idx = Just $ unsafeCoerce ta
  | otherwise = Nothing

newtype P (t :: * -> *) (r :: [* -> *]) = P { unP :: Word }

class FindElem (t :: * -> *) (r :: [* -> *]) where
  elemIdx :: P t r

instance FindElem t (t ': r) where
  elemIdx = P 0

instance {-# OVERLAPPABLE #-}(FindElem t r) => FindElem t (t' ': r) where
  elemIdx = P $ 1 + unP (elemIdx :: P t r)

class FindElem eff effs => Member (eff :: * -> *)  effs where
  inj :: eff a -> Union effs a

  prj :: Union effs a -> Maybe (eff a)

instance FindElem eff effs => Member eff effs where
  inj = unsafeInj $ unP (elemIdx :: P eff effs)

  prj = unsafePrj $ unP (elemIdx :: P eff effs)