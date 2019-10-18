{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypeLevel
  (
  )
where

import Data.Kind
import Data.Text
import GHC.Generics
import GHC.TypeLits

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All _ '[] = ()
  All c (t ': ts) = (c t, All c ts)

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data MapList :: (a -> Exp b) -> [a] -> Exp [b]

type instance Eval (MapList f '[]) = '[]

type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

data Snd :: (a, b) -> Exp b

type instance Eval (Snd '(a, b)) = b

-- ----------------------------

-- data T1 a

-- type family Break (c :: Constraint) (t :: Type -> Type) :: Constraint where
--   Break _ T1 = ((), ())
--   Break _ _ = ()

-- type family NoGeneric (t :: Type -> Type) where
--   NoGeneric x = TypeError ('Text "No instance for " ':<>: 'ShowType (Generic x))

-- foo :: Break (NoGeneric t) t => t -> t
-- foo t = t
