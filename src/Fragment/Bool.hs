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
module Fragment.Bool (
    TyFBool(..)
  , AsTyBool(..)
  , TmFBool(..)
  , AsTmBool(..)
  , boolFragment
  , tyBool
  , tmBool
  , tmAnd
  , tmOr
  ) where

import Control.Monad.Except (MonadError)

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Error

data TyFBool (f :: * -> *) a =
  TyBoolF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFBool

instance Eq1 (TyFBool f) where
  liftEq = $(makeLiftEq ''TyFBool)

instance Ord1 (TyFBool f) where
  liftCompare = $(makeLiftCompare ''TyFBool)

instance Show1 (TyFBool f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFBool)

instance Bound TyFBool where
  TyBoolF >>>= _ = TyBoolF

data TmFBool f a =
    TmBoolF Bool
  | TmAndF (f a) (f a)
  | TmOrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFBool

instance (Eq1 f) => Eq1 (TmFBool f) where
  liftEq = $(makeLiftEq ''TmFBool)

instance (Ord1 f) => Ord1 (TmFBool f) where
  liftCompare = $(makeLiftCompare ''TmFBool)

instance (Show1 f) => Show1 (TmFBool f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFBool)

instance Bound TmFBool where
  TmBoolF b >>>= _ = TmBoolF b
  TmAndF x y >>>= f = TmAndF (x >>= f) (y >>= f)
  TmOrF x y >>>= f = TmOrF (x >>= f) (y >>= f)

class AsTyBool ty where
  _TyBoolP :: Prism' (ty a) (TyFBool ty a)

  _TyBool :: Prism' (ty a) ()
  _TyBool = _TyBoolP . _TyBoolF

instance AsTyBool f => AsTyBool (TyFBool f) where
  _TyBoolP = id . _TyBoolP

class AsTmBool tm where
  _TmBoolP :: Prism' (tm a) (TmFBool tm a)

  _TmBool :: Prism' (tm a) Bool
  _TmBool = _TmBoolP . _TmBoolF

  _TmAnd :: Prism' (tm a) (tm a, tm a)
  _TmAnd = _TmBoolP . _TmAndF

  _TmOr :: Prism' (tm a) (tm a, tm a)
  _TmOr = _TmBoolP . _TmOrF

instance AsTmBool f => AsTmBool (TmFBool f) where
  _TmBoolP = id . _TmBoolP

valBool :: AsTmBool tm => tm a -> Maybe (tm a)
valBool tm = do
  _ <- preview _TmBool tm
  return tm

stepAnd1 :: AsTmBool tm
         => (tm a -> Maybe (tm a))
         -> tm a
         -> Maybe (tm a)
stepAnd1 stepFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  tm1' <- stepFn tm1
  return . review _TmAnd $ (tm1', tm2)

stepAnd2 :: AsTmBool tm
         => (tm a -> Maybe (tm a))
         -> tm a
         -> Maybe (tm a)
stepAnd2 stepFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  _ <- preview _TmBool tm1
  tm2' <- stepFn tm2
  return . review _TmAnd $ (tm1, tm2')

stepAndBool :: AsTmBool tm
           => tm a
           -> Maybe (tm a)
stepAndBool tm = do
  (tm1, tm2) <- preview _TmAnd tm
  b1 <- preview _TmBool tm1
  b2 <- preview _TmBool tm2
  return . review _TmBool $ b1 && b2

stepOr1 :: AsTmBool tm
         => (tm a -> Maybe (tm a))
         -> tm a
         -> Maybe (tm a)
stepOr1 stepFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  tm1' <- stepFn tm1
  return . review _TmOr $ (tm1', tm2)

stepOr2 :: AsTmBool tm
         => (tm a -> Maybe (tm a))
         -> tm a
         -> Maybe (tm a)
stepOr2 stepFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  _ <- preview _TmBool tm1
  tm2' <- stepFn tm2
  return . review _TmOr $ (tm1, tm2')

stepOrBool :: AsTmBool tm
           => tm a
           -> Maybe (tm a)
stepOrBool tm = do
  (tm1, tm2) <- preview _TmOr tm
  b1 <- preview _TmBool tm1
  b2 <- preview _TmBool tm2
  return . review _TmBool $ b1 || b2

inferBool :: (Monad m, AsTyBool ty, AsTmBool tm)
         => tm a
         -> Maybe (m (ty a))
inferBool tm = do
  _ <- preview _TmBool tm
  return . return . review _TyBool $ ()

inferAnd :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsTyBool ty, AsTmBool tm)
         => (tm a -> m (ty a))
         -> tm a
         -> Maybe (m (ty a))
inferAnd inferFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  return $ do
    let ty = review _TyBool ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

inferOr :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsTyBool ty, AsTmBool tm)
         => (tm a -> m (ty a))
         -> tm a
         -> Maybe (m (ty a))
inferOr inferFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  return $ do
    let ty = review _TyBool ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

boolFragment :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsTyBool ty, AsTmBool tm)
            => FragmentInput e s r m ty p tm a
boolFragment =
  FragmentInput
    [ValueBase valBool]
    [ EvalStep stepAnd1
    , EvalStep stepAnd2
    , EvalBase stepAndBool
    , EvalStep stepOr1
    , EvalStep stepOr2
    , EvalBase stepOrBool
    ]
    [ InferBase inferBool
    , InferRecurse inferAnd
    , InferRecurse inferOr
    ]
    [] []

-- Helpers

tyBool :: AsTyBool ty => ty a
tyBool = review _TyBool ()

tmBool :: AsTmBool tm => Bool -> tm a
tmBool = review _TmBool

tmAnd :: AsTmBool tm => tm a -> tm a -> tm a
tmAnd = curry $ review _TmAnd

tmOr :: AsTmBool tm => tm a -> tm a -> tm a
tmOr = curry $ review _TmOr
