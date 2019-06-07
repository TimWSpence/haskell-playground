{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map              as Map
import qualified Data.Set              as Set

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  -- | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TFun Type Type
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

data Env = TypeEnv { types :: Map.Map Name Scheme }
  deriving (Eq, Show)

typeInt, typeBool :: Type
typeInt  = TCon "Int"
typeBool = TCon "Bool"

-- | Inference monad
type Infer a = (ReaderT
                  Env             -- Typing environment
                  (StateT         -- Inference state
                  InferState
                  (Except         -- Inference errors
                    TypeError))
                  a)              -- Result

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type] deriving (Show)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply (Subst s) t@(TVar n) = Map.findWithDefault t n s
  apply _ t@(TCon _)         = t
  apply s (TFun t1 t2)       = (apply s t1) `TFun` (apply s t2)

  ftv TCon{}       = Set.empty
  ftv (TVar(a))    = Set.singleton a
  ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply s as = apply s <$> as

  ftv = foldr (Set.union .ftv) Set.empty

instance Substitutable Constraint where
  apply s = fmap (apply s)

  ftv (t1,t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall tvars t) = Forall tvars (apply s' t)
    where
      s' = Subst $ foldr Map.delete s tvars

  ftv (Forall tvars t) = (ftv t) `Set.difference` (Set.fromList tvars)

instance Substitutable Env where
  apply s (TypeEnv types) = TypeEnv $ Map.map (apply s) types

  ftv (TypeEnv types) = ftv $ Map.elems types

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

lookupEnv :: Name -> Infer Type
lookupEnv name = do
  env <- reader types
  case Map.lookup name env of
    Nothing     -> throwError $ UnboundVariable name
    Just scheme -> instantiate scheme

instantiate :: Scheme -> Infer Type
instantiate (Forall tvars t) = do
  vs <- mapM (const fresh) tvars
  let s = Subst $ Map.fromList $ zip tvars vs
  return $ apply s t

generalise :: Env -> Type -> Scheme
generalise env t =
  let vs = Set.toList $ ftv t `Set.difference` ftv env in Forall vs t

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (name, scheme) inf = do
  env <- ask
  let scope e = (remove name e) `extend` (name, scheme)
  local scope inf

remove :: Name -> Env -> Env
remove name (TypeEnv env )= TypeEnv $ Map.delete name env

extend :: Env -> (Name, Scheme) -> Env
extend (TypeEnv env) (name, scheme) = TypeEnv $ Map.insert name scheme env

infer :: Expr -> Infer (Type, [Constraint])
infer expr = case expr of
  Lit (LInt _) -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])

  (Var name) -> do
    t <- lookupEnv name
    return (t, [])

  (App e1 e2) -> do
    tv        <- fresh
    (t1, cs1) <- infer e1
    (t2, cs2) <- infer e2
    return $ (tv, cs1 ++ cs2 ++ [(t1, t2 `TFun` tv)] )

  (Lam name e1) -> do
    tv <- fresh
    (t,c) <- inEnv (name, Forall [] tv) $ infer e1
    return (tv `TFun` t, c)

  (Let name e1 e2) -> do
    env <- ask
    (t1, cs1) <- infer e1
    let scheme = generalise env t1
    (t2, cs2) <- inEnv (name, scheme) $ infer e2
    return (t2, cs1 ++ cs2)

  (If e1 e2 e3) -> do
    (t1, cs1) <- infer e1
    (t2, cs2) <- infer e2
    (t3, cs3) <- infer e3
    return (t2, cs1 ++ cs2 ++ cs3 ++ [(t1, typeBool), (t2, t3)])
  -- | Fix Expr
  (Op op e1 e2) -> do
    (t1, cs1) <- infer e1
    (t2, cs2) <- infer e2
    let c = if op == Eql then [] else [(t1, typeInt), (t2, typeInt)]
    return (if op == Eql then typeBool else typeInt, cs1 ++ cs2 ++ c)

runInfer :: Env -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env inf = runExcept (evalStateT (runReaderT inf env) initInfer)

ops :: Binop -> Type
ops Add = typeInt `TFun` (typeInt `TFun` typeInt)
ops Sub = typeInt `TFun` (typeInt `TFun` typeInt)
ops Mul = typeInt `TFun` (typeInt `TFun` typeInt)
ops Eql = typeInt `TFun` (typeInt `TFun` typeBool)

compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity . runExceptT $ solver (emptySubst, cs)

solver :: Unifier -> Solve Subst
solver (sub, []) = return sub
solver (sub, ((t1,t2):r)) = do
  s1 <- unifies t1 t2
  solver ((s1 `compose` sub), apply s1 r)

emptySubst :: Subst
emptySubst = Subst $ Map.empty

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return $ emptySubst
unifies t (TVar v) = bind v t
unifies (TVar v) t = bind v t
unifies (TFun f1 t1) (TFun f2 t2) = unifyMany [f1, t1] [f2, t2]
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1:r1) (t2:r2) = do
  s <- unifies t1 t2
  s2 <- unifyMany ((apply s) <$> r1) ((apply s) <$> r2)
  return $ s2 `compose` s
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

bind :: TVar -> Type  -> Solve Subst
bind v t | t == TVar v = return emptySubst
         | occursCheck v t = throwError $ InfiniteType v t
         | otherwise = return $ Subst $ Map.singleton v t

occursCheck :: TVar -> Type -> Bool
occursCheck v t = v `Set.member` ftv t

inferExpr :: Env -> Expr -> Either TypeError Scheme
inferExpr env expr = case runInfer env (infer expr) of
  Left e -> Left e
  Right (tpe, cs) -> case runSolve cs of
    Left e -> Left e
    Right sub -> Right . closeOver $ apply sub tpe

closeOver :: Type -> Scheme
closeOver = generalise emptyEnv

emptyEnv :: Env
emptyEnv = TypeEnv $ Map.empty
