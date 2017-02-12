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
{-# LANGUAGE KindSignatures #-}
module Fragment.Int (
    TyFInt(..)
  , AsTyInt(..)
  , TmFInt(..)
  , AsTmInt(..)
  , intFragment
  , tyInt
  , tmInt
  , tmAdd
  , tmMul
  ) where

import Control.Monad.Except (MonadError)

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Error

data TyFInt (f :: * -> *) a =
  TyIntF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFInt

instance Eq1 (TyFInt f) where
  liftEq = $(makeLiftEq ''TyFInt)

instance Ord1 (TyFInt f) where
  liftCompare = $(makeLiftCompare ''TyFInt)

instance Show1 (TyFInt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFInt)

instance Bound TyFInt where
  TyIntF >>>= _ = TyIntF

data TmFInt f a =
    TmIntF Int
  | TmAddF (f a) (f a)
  | TmMulF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFInt

instance (Eq1 f) => Eq1 (TmFInt f) where
  liftEq = $(makeLiftEq ''TmFInt)

instance (Ord1 f) => Ord1 (TmFInt f) where
  liftCompare = $(makeLiftCompare ''TmFInt)

instance (Show1 f) => Show1 (TmFInt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFInt)

instance Bound TmFInt where
  TmIntF b >>>= _ = TmIntF b
  TmAddF x y >>>= f = TmAddF (x >>= f) (y >>= f)
  TmMulF x y >>>= f = TmMulF (x >>= f) (y >>= f)

class AsTyInt ty where
  _TyIntP :: Prism' (ty a) (TyFInt ty a)

  _TyInt :: Prism' (ty a) ()
  _TyInt = _TyIntP . _TyIntF

instance AsTyInt f => AsTyInt (TyFInt f) where
  _TyIntP = id . _TyIntP

class AsTmInt tm where
  _TmIntP :: Prism' (tm a) (TmFInt tm a)

  _TmInt :: Prism' (tm a) Int
  _TmInt = _TmIntP . _TmIntF

  _TmAdd :: Prism' (tm a) (tm a, tm a)
  _TmAdd = _TmIntP . _TmAddF

  _TmMul :: Prism' (tm a) (tm a, tm a)
  _TmMul = _TmIntP . _TmMulF

instance AsTmInt f => AsTmInt (TmFInt f) where
  _TmIntP = id . _TmIntP

valInt :: AsTmInt tm => tm a -> Maybe (tm a)
valInt tm = do
  _ <- preview _TmInt tm
  return tm

stepAdd1 :: AsTmInt tm
         => (tm a -> Maybe (tm a))
         -> tm a
         -> Maybe (tm a)
stepAdd1 stepFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  tm1' <- stepFn tm1
  return . review _TmAdd $ (tm1', tm2)

stepAdd2 :: AsTmInt tm
         => (tm a -> Maybe (tm a))
         -> tm a
         -> Maybe (tm a)
stepAdd2 stepFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  _ <- preview _TmInt tm1
  tm2' <- stepFn tm2
  return . review _TmAdd $ (tm1, tm2')

stepAddInt :: AsTmInt tm
           => tm a
           -> Maybe (tm a)
stepAddInt tm = do
  (tm1, tm2) <- preview _TmAdd tm
  i1 <- preview _TmInt tm1
  i2 <- preview _TmInt tm2
  return . review _TmInt $ i1 + i2

stepMul1 :: AsTmInt tm
         => (tm a -> Maybe (tm a))
         -> tm a
         -> Maybe (tm a)
stepMul1 stepFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  tm1' <- stepFn tm1
  return . review _TmMul $ (tm1', tm2)

stepMul2 :: AsTmInt tm
         => (tm a -> Maybe (tm a))
         -> tm a
         -> Maybe (tm a)
stepMul2 stepFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  _ <- preview _TmInt tm1
  tm2' <- stepFn tm2
  return . review _TmMul $ (tm1, tm2')

stepMulInt :: AsTmInt tm
           => tm a
           -> Maybe (tm a)
stepMulInt tm = do
  (tm1, tm2) <- preview _TmMul tm
  i1 <- preview _TmInt tm1
  i2 <- preview _TmInt tm2
  return . review _TmInt $ i1 * i2

inferInt :: (Monad m, AsTyInt ty, AsTmInt tm)
         => tm a
         -> Maybe (m (ty a))
inferInt tm = do
  _ <- preview _TmInt tm
  return . return . review _TyInt $ ()

inferAdd :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsTyInt ty, AsTmInt tm)
         => (tm a -> m (ty a))
         -> tm a
         -> Maybe (m (ty a))
inferAdd inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    let ty = review _TyInt ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

inferMul :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsTyInt ty, AsTmInt tm)
         => (tm a -> m (ty a))
         -> tm a
         -> Maybe (m (ty a))
inferMul inferFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  return $ do
    let ty = review _TyInt ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

intFragment :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsTyInt ty, AsTmInt tm)
            => FragmentInput e s r m ty tm a
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

-- Helpers

tyInt :: AsTyInt ty => ty a
tyInt = review _TyInt ()

tmInt :: AsTmInt tm => Int -> tm a
tmInt = review _TmInt

tmAdd :: AsTmInt tm => tm a -> tm a -> tm a
tmAdd = curry $ review _TmAdd

tmMul :: AsTmInt tm => tm a -> tm a -> tm a
tmMul = curry $ review _TmMul

