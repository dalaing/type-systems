{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Int (
    TyFInt(..)
  , AsTyInt(..)
  , PtFInt(..)
  , AsPtInt(..)
  , TmFInt(..)
  , AsTmInt(..)
  , IntContext
  , intFragment
  , tyInt
  , ptInt
  , tmInt
  , tmAdd
  , tmMul
  ) where

import Control.Monad (MonadPlus(..))

import Control.Monad.Except (MonadError)

import Control.Lens

import Bound
import Data.Deriving

import Fragment
import Fragment.Ast
import Error
import Util

data TyFInt (f :: * -> *) a =
  TyIntF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFInt

deriveEq1 ''TyFInt
deriveOrd1 ''TyFInt
deriveShow1 ''TyFInt

instance EqRec TyFInt where
  liftEqRec _ _ TyIntF TyIntF = True

instance OrdRec TyFInt where
  liftCompareRec _ _ TyIntF TyIntF = EQ

instance ShowRec TyFInt where
  liftShowsPrecRec _ _ _ _ n TyIntF = showsPrec n TyIntF

instance Bound TyFInt where
  TyIntF >>>= _ = TyIntF

instance Bitransversable TyFInt where
  bitransverse _ _ TyIntF = pure TyIntF

class AsTyInt ty where
  _TyIntP :: Prism' (ty k a) (TyFInt k a)

  _TyInt :: Prism' (Type ty a) ()
  _TyInt = _TyTree . _TyIntP . _TyIntF

instance AsTyInt TyFInt where
  _TyIntP = id

data PtFInt (f :: * -> *) a =
  PtIntF Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFInt

deriveEq1 ''PtFInt
deriveOrd1 ''PtFInt
deriveShow1 ''PtFInt

instance Bound PtFInt where
  PtIntF i >>>= _ = PtIntF i

instance Bitransversable PtFInt where
  bitransverse _ _ (PtIntF i) = pure $ PtIntF i

class AsPtInt pt where
  _PtIntP :: Prism' (pt k a) (PtFInt k a)

  _PtInt :: Prism' (Pattern pt a) Int
  _PtInt = _PtTree . _PtIntP . _PtIntF

instance AsPtInt PtFInt where
  _PtIntP = id

data TmFInt (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmIntF Int
  | TmAddF (f a) (f a)
  | TmMulF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFInt

deriveEq1 ''TmFInt
deriveOrd1 ''TmFInt
deriveShow1 ''TmFInt

instance Bound (TmFInt ty pt) where
  TmIntF b >>>= _ = TmIntF b
  TmAddF x y >>>= f = TmAddF (x >>= f) (y >>= f)
  TmMulF x y >>>= f = TmMulF (x >>= f) (y >>= f)

instance Bitransversable (TmFInt ty pt) where
  bitransverse _ _ (TmIntF i) = pure $ TmIntF i
  bitransverse fT fL (TmAddF x y) = TmAddF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmMulF x y) = TmMulF <$> fT fL x <*> fT fL y

class AsTmInt ty pt tm where
  _TmIntP :: Prism' (tm ty pt k a) (TmFInt ty pt k a)

  _TmInt :: Prism' (Term ty pt tm a) Int
  _TmInt = _Wrapped . _ATerm . _TmIntP . _TmIntF

  _TmAdd :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmAdd = _Wrapped . _ATerm . _TmIntP . _TmAddF . bimapping _Unwrapped _Unwrapped

  _TmMul :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmMul = _Wrapped . _ATerm . _TmIntP . _TmMulF . bimapping _Unwrapped _Unwrapped

instance AsTmInt ty pt TmFInt where
  _TmIntP = id

valInt :: AsTmInt ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
valInt tm = do
  _ <- preview _TmInt tm
  return tm

stepAdd1 :: AsTmInt ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepAdd1 stepFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  tm1' <- stepFn tm1
  return . review _TmAdd $ (tm1', tm2)

stepAdd2 :: AsTmInt ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepAdd2 stepFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  _ <- preview _TmInt tm1
  tm2' <- stepFn tm2
  return . review _TmAdd $ (tm1, tm2')

stepAddInt :: AsTmInt ty pt tm
           => Term ty pt tm a
           -> Maybe (Term ty pt tm a)
stepAddInt tm = do
  (tm1, tm2) <- preview _TmAdd tm
  i1 <- preview _TmInt tm1
  i2 <- preview _TmInt tm2
  return . review _TmInt $ i1 + i2

stepMul1 :: AsTmInt ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepMul1 stepFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  tm1' <- stepFn tm1
  return . review _TmMul $ (tm1', tm2)

stepMul2 :: AsTmInt ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepMul2 stepFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  _ <- preview _TmInt tm1
  tm2' <- stepFn tm2
  return . review _TmMul $ (tm1, tm2')

stepMulInt :: AsTmInt ty pt tm
           => Term ty pt tm a
           -> Maybe (Term ty pt tm a)
stepMulInt tm = do
  (tm1, tm2) <- preview _TmMul tm
  i1 <- preview _TmInt tm1
  i2 <- preview _TmInt tm2
  return . review _TmInt $ i1 * i2

inferInt :: (Monad m, AsTyInt ty, AsTmInt ty pt tm)
         => Term ty pt tm a
         -> Maybe (m (Type ty a))
inferInt tm = do
  _ <- preview _TmInt tm
  return . return . review _TyInt $ ()

inferAdd :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyInt ty, AsTmInt ty pt tm)
         => (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferAdd inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    let ty = review _TyInt ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

inferMul :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyInt ty, AsTmInt ty pt tm)
         => (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferMul inferFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  return $ do
    let ty = review _TyInt ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

matchInt :: (AsPtInt pt, AsTmInt ty pt tm)
         => (Term ty pt tm a -> Term ty pt tm a)
         -> Pattern pt a
         -> Term ty pt tm a
         -> Maybe [Term ty pt tm a]
matchInt eval p tm = do
  i <- preview _PtInt p
  j <- preview _TmInt (eval tm)
  if i == j
  then return []
  else mzero

checkInt :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsPtInt pt, AsTyInt ty) => Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkInt p ty = do
  _ <- preview _PtInt p
  return $ do
    let tyI = review _TyInt ()
    expect tyI ty
    return []

type IntContext e s r m ty pt tm a = (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyInt ty, AsPtInt pt, AsTmInt ty pt tm)

intFragment :: IntContext e s r m ty p tm a
            => FragmentInput e s r m ty p tm a
intFragment =
  FragmentInput
    [ValueBase valInt]
    [ EvalStep stepAdd1
    , EvalStep stepAdd2
    , EvalBase stepAddInt
    , EvalStep stepMul1
    , EvalStep stepMul2
    , EvalBase stepMulInt
    ]
    [ InferBase inferInt
    , InferRecurse inferAdd
    , InferRecurse inferMul
    ]
    [ PMatchEval matchInt ]
    [ PCheckBase checkInt ]

-- Helpers

tyInt :: AsTyInt ty => Type ty a
tyInt = review _TyInt ()

ptInt :: AsPtInt pt => Int -> Pattern pt a
ptInt = review _PtInt

tmInt :: AsTmInt ty pt tm => Int -> Term ty pt tm a
tmInt = review _TmInt

tmAdd :: AsTmInt ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmAdd = curry $ review _TmAdd

tmMul :: AsTmInt ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmMul = curry $ review _TmMul

