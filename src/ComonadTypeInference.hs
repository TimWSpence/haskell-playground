-- Inspired by https://brianmckenna.org/blog/type_annotation_cofree
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding (sequence)

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.State hiding (sequence)
import Data.Foldable (Foldable, fold)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Traversable (Traversable, sequence)
import qualified Data.Map as M
import Data.Functor.Classes

import Data.Deriving

data AST a = ALambda String a
             | AApply a a
             | ANumber Int
             | AString String
             | AIdent String


deriving instance Show a => Show (AST a)
deriving instance Functor AST
deriving instance Foldable AST
deriving instance Traversable AST
deriving instance Eq a => Eq (AST a)
deriving instance Ord a => Ord (AST a)

deriveEq1 ''AST
deriveOrd1 ''AST
deriveShow1 ''AST

newtype Mu f = Mu (f (Mu f))

example :: Mu AST
example = Mu $ AApply (Mu $ ALambda "x" (Mu $ AIdent "x")) (Mu $ ANumber 2)

data Type = TLambda Type Type
            | TVar Int
            | TNumber
            | TString

deriving instance Show Type

data Constraint = EqualityConstraint Type Type

deriving instance Show Constraint

data TypeResult = TypeResult {
  constraints :: [Constraint],
  assumptions :: M.Map String [Type]
                             }

deriving instance Show TypeResult

instance Semigroup TypeResult where
  (<>) = mappend

instance Monoid TypeResult where
  mempty = TypeResult {
    constraints = mempty,
    assumptions = mempty
                      }

  mappend a b = TypeResult {
    constraints = constraints a <> constraints b,
    assumptions = assumptions a <> assumptions b
                           }

data TypeState t m = TypeState {
  varId :: Int,
  memo :: M.Map t m
                               }


type TypeCheck t = State (TypeState t (Type, TypeResult)) (Type, TypeResult)

freshVarId :: State (TypeState t m) Type
freshVarId = do
  v <- gets varId
  modify (\s -> s { varId = succ v})
  return $ TVar v

memoizedTC :: Ord c => (c -> TypeCheck c) -> c -> TypeCheck c
memoizedTC f c = gets memo >>= maybe memoize return . M.lookup c where
  memoize = do
    r <- f c
    modify (\s -> s { memo = M.insert c r (memo s)})
    return r

cofreeMu :: Functor f => Mu f -> Cofree f ()
cofreeMu (Mu f) = () :< (cofreeMu <$> f)

attribute :: Cofree AST () -> Cofree AST (Type, TypeResult)
attribute c = let initial = TypeState {memo = M.empty, varId = 0}
              in evalState (sequence $ extend (memoizedTC generateConstraints) c) initial


generateConstraints :: Cofree AST () -> TypeCheck (Cofree AST ())
generateConstraints (() :< AString _) = return (TString, mempty)
generateConstraints (() :< ANumber _) = return (TNumber, mempty)
generateConstraints (() :< AIdent ident) = do
  var <- freshVarId
  return (var, TypeResult {
             constraints = mempty,
             assumptions = M.singleton ident [var]
                          })
generateConstraints (() :< ALambda s b) = do
  var <- freshVarId
  br  <- memoizedTC generateConstraints b
  let cs = maybe [] (fmap (EqualityConstraint var)) (M.lookup s . assumptions $ snd br)
      as = M.delete s . assumptions $ snd br
  return (TLambda var (fst br), TypeResult {
    constraints = constraints (snd br) <> cs,
    assumptions = as
  })
generateConstraints (() :< AApply a b) = do
  var <- freshVarId
  ar  <- memoizedTC generateConstraints a
  br  <- memoizedTC generateConstraints b
  return (var, TypeResult {
             constraints = [EqualityConstraint (fst ar) (TLambda (fst br) var)],
             assumptions = mempty
                          })

solveConstraints :: [Constraint] -> Maybe (M.Map Int Type)
solveConstraints = foldl (\b a -> liftM2 mappend (solve b a) b) $ Just M.empty
  where
    solve maybeSubs (EqualityConstraint a b) = do
      subs <- maybeSubs
      mostGeneralUnifier (substitute subs a) (substitute subs b)

mostGeneralUnifier :: Type -> Type -> Maybe (M.Map Int Type)
mostGeneralUnifier (TVar i) b = Just $ M.singleton i b
mostGeneralUnifier a (TVar i) = Just $ M.singleton i a
mostGeneralUnifier TString TString = Just M.empty
mostGeneralUnifier TNumber TNumber = Just M.empty
mostGeneralUnifier (TLambda a b) (TLambda c d) = do
  s1 <- mostGeneralUnifier a c
  liftM2 mappend (mostGeneralUnifier (substitute s1 b) (substitute s1 d)) $ Just s1
mostGeneralUnifier _ _ = Nothing

substitute :: M.Map Int Type -> Type -> Type
substitute subs v@(TVar i) = maybe v (substitute subs) $ M.lookup i subs
substitute subs (TLambda a b) = TLambda (substitute subs a) (substitute subs b)
substitute _ t = t

typeTree :: Cofree AST () -> Maybe (Cofree AST Type)
typeTree c =
  let result = attribute c
      (r :< _) = result
      maybeSubs = solveConstraints . constraints $ snd r
  in fmap (\subs -> fmap (substitute subs . fst) result) maybeSubs

main :: IO ()
main = do
  print $ cofreeMu example
  print . attribute $ cofreeMu example
  print . typeTree $ cofreeMu example
