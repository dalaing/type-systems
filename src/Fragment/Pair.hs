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
{-# LANGUAGE OverloadedStrings #-}
module Fragment.Pair (
    TyFPair(..)
  , AsTyPair(..)
  , TmFPair(..)
  , AsTmPair(..)
  , AsExpectedTyPair(..)
  , pairFragmentLazy
  , pairFragmentStrict
  , tyPair
  , tmPair
  , tmFst
  , tmSnd
  ) where

import Control.Monad.Except (MonadError)

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment

data TyFPair f a =
  TyPairF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFPair

instance Eq1 f => Eq1 (TyFPair f) where
  liftEq = $(makeLiftEq ''TyFPair)

instance Ord1 f => Ord1 (TyFPair f) where
  liftCompare = $(makeLiftCompare ''TyFPair)

instance Show1 f => Show1 (TyFPair f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFPair)

instance Bound TyFPair where
  TyPairF x y >>>= f = TyPairF (x >>= f) (y >>= f)

data TmFPair f a =
    TmPairF (f a) (f a)
  | TmFstF (f a)
  | TmSndF (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFPair

instance Eq1 f => Eq1 (TmFPair f) where
  liftEq = $(makeLiftEq ''TmFPair)

instance Ord1 f => Ord1 (TmFPair f) where
  liftCompare = $(makeLiftCompare ''TmFPair)

instance Show1 f => Show1 (TmFPair f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFPair)

instance Bound TmFPair where
  TmPairF x y >>>= f = TmPairF (x >>= f) (y >>= f)
  TmFstF x >>>= f = TmFstF (x >>= f)
  TmSndF x >>>= f = TmSndF (x >>= f)

class AsTyPair ty where
  _TyPairP :: Prism' (ty a) (TyFPair ty a)

  _TyPair :: Prism' (ty a) (ty a, ty a)
  _TyPair = _TyPairP . _TyPairF

instance AsTyPair f => AsTyPair (TyFPair f) where
  _TyPairP = id . _TyPairP

class AsTmPair tm where
  _TmPairP :: Prism' (tm a) (TmFPair tm a)

  _TmPair :: Prism' (tm a) (tm a, tm a)
  _TmPair = _TmPairP . _TmPairF

  _TmFst :: Prism' (tm a) (tm a)
  _TmFst = _TmPairP . _TmFstF

  _TmSnd :: Prism' (tm a) (tm a)
  _TmSnd = _TmPairP . _TmSndF

instance AsTmPair f => AsTmPair (TmFPair f) where
  _TmPairP = id . _TmPairP

-- Errors

class AsExpectedTyPair e ty | e -> ty where
  _ExpectedTyPair :: Prism' e ty

expectTyPair :: (MonadError e m, AsExpectedTyPair e (ty a), AsTyPair ty) => ty a -> m (ty a, ty a)
expectTyPair ty =
  case preview _TyPair ty of
    Just (ty1, ty2) -> return (ty1, ty2)
    _ -> throwing _ExpectedTyPair ty

-- Rules

stepFstLazy :: AsTmPair tm => tm a -> Maybe (tm a)
stepFstLazy tm = do
  tmP <- preview _TmFst tm
  (tm1, _) <- preview _TmPair tmP
  return tm1

stepSndLazy :: AsTmPair tm => tm a -> Maybe (tm a)
stepSndLazy tm = do
  tmP <- preview _TmSnd tm
  (_, tm2) <- preview _TmPair tmP
  return tm2

-- TODO check this, there might be more rules
evalRulesLazy :: AsTmPair tm => FragmentInput e s r m ty tm a
evalRulesLazy =
  FragmentInput [] [EvalBase stepFstLazy, EvalBase stepSndLazy] []

valueFst :: AsTmPair tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
valueFst valueFn tm = do
  tmP <- preview _TmFst tm
  (tm1, tm2) <- preview _TmPair tmP
  v1 <- valueFn tm1
  _ <- valueFn tm2
  return v1

valueSnd :: AsTmPair tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
valueSnd valueFn tm = do
  tmP <- preview _TmFst tm
  (tm1, tm2) <- preview _TmPair tmP
  _ <- valueFn tm1
  v2 <- valueFn tm2
  return v2

stepFstStrict :: AsTmPair tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepFstStrict stepFn tm = do
  tmP <- preview _TmFst tm
  tmP' <- stepFn tmP
  return $ review _TmFst tmP'

stepSndStrict :: AsTmPair tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepSndStrict stepFn tm = do
  tmP <- preview _TmSnd tm
  tmP' <- stepFn tmP
  return $ review _TmSnd tmP'

stepPair1 :: AsTmPair tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepPair1 stepFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  tm1' <- stepFn tm1
  return $ review _TmPair (tm1', tm2)

stepPair2 :: AsTmPair tm => (tm a -> Maybe (tm a)) -> (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepPair2 valueFn stepFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  v1 <- valueFn tm1
  tm2' <- stepFn tm2
  return $ review _TmPair (v1, tm2')

evalRulesStrict :: AsTmPair tm => FragmentInput e s r m ty tm a
evalRulesStrict =
  FragmentInput
    [ ValueRecurse valueFst
    , ValueRecurse valueSnd
    ]
    [ EvalStep stepFstStrict
    , EvalStep stepSndStrict
    , EvalStep stepPair1
    , EvalValueStep stepPair2
    ]
    []

inferTmPair :: (Monad m, AsTyPair ty, AsTmPair tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmPair inferFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    return $ review _TyPair (ty1, ty2)

inferTmFst :: (MonadError e m, AsExpectedTyPair e (ty a), AsTyPair ty, AsTmPair tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmFst inferFn tm = do
  tmP <- preview _TmFst tm
  return $ do
    tyP <- inferFn tmP
    (ty1, _) <- expectTyPair tyP
    return ty1

inferTmSnd :: (MonadError e m, AsExpectedTyPair e (ty a), AsTyPair ty, AsTmPair tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmSnd inferFn tm = do
  tmP <- preview _TmFst tm
  return $ do
    tyP <- inferFn tmP
    (_, ty2) <- expectTyPair tyP
    return ty2

inferRules :: (MonadError e m, AsExpectedTyPair e (ty a), AsTyPair ty, AsTmPair tm) => FragmentInput e s r m ty tm a
inferRules =
  FragmentInput
    []
    []
    [ InferRecurse inferTmPair
    , InferRecurse inferTmFst
    , InferRecurse inferTmSnd
    ]

pairFragmentLazy :: (MonadError e m, AsExpectedTyPair e (ty a), AsTyPair ty, AsTmPair tm)
             => FragmentInput e s r m ty tm a
pairFragmentLazy =
  mappend evalRulesLazy inferRules

pairFragmentStrict :: (MonadError e m, AsExpectedTyPair e (ty a), AsTyPair ty, AsTmPair tm)
             => FragmentInput e s r m ty tm a
pairFragmentStrict =
  mappend evalRulesStrict inferRules

-- Helpers

tyPair :: AsTyPair ty => ty a -> ty a -> ty a
tyPair = curry $ review _TyPair

tmPair :: AsTmPair tm => tm a -> tm a -> tm a
tmPair = curry $ review _TmPair

tmFst :: AsTmPair tm => tm a -> tm a
tmFst = review _TmFst

tmSnd :: AsTmPair tm => tm a -> tm a
tmSnd = review _TmSnd
