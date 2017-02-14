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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Fragment.Tuple (
    TyFTuple(..)
  , AsTyTuple(..)
  , PtFTuple(..)
  , AsPtTuple(..)
  , TmFTuple(..)
  , AsTmTuple(..)
  , AsExpectedTyTuple(..)
  , AsTupleOutOfBounds(..)
  , TupleContext
  , tupleFragmentLazy
  , tupleFragmentStrict
  , tyTuple
  , ptTuple
  , tmTuple
  , tmTupleIx
  ) where

import Control.Monad (zipWithM)
import Data.List (splitAt)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError)

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment

data TyFTuple f a =
  TyTupleF [f a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFTuple

instance Eq1 f => Eq1 (TyFTuple f) where
  liftEq = $(makeLiftEq ''TyFTuple)

instance Ord1 f => Ord1 (TyFTuple f) where
  liftCompare = $(makeLiftCompare ''TyFTuple)

instance Show1 f => Show1 (TyFTuple f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFTuple)

instance Bound TyFTuple where
  TyTupleF tys >>>= f = TyTupleF (fmap (>>= f) tys)

class AsTyTuple ty where
  _TyTupleP :: Prism' (ty a) (TyFTuple ty a)

  _TyTuple :: Prism' (ty a) [ty a]
  _TyTuple = _TyTupleP . _TyTupleF

instance AsTyTuple f => AsTyTuple (TyFTuple f) where
  _TyTupleP = id . _TyTupleP

data PtFTuple f a =
  PtTupleF [f a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFTuple

instance Eq1 f => Eq1 (PtFTuple f) where
  liftEq = $(makeLiftEq ''PtFTuple)

instance Ord1 f => Ord1 (PtFTuple f) where
  liftCompare = $(makeLiftCompare ''PtFTuple)

instance Show1 f => Show1 (PtFTuple f) where
  liftShowsPrec = $(makeLiftShowsPrec ''PtFTuple)

class AsPtTuple pt where
  _PtTupleP :: Prism' (pt a) (PtFTuple pt a)

  _PtTuple :: Prism' (pt a) [pt a]
  _PtTuple = _PtTupleP . _PtTupleF

instance AsPtTuple pt => AsPtTuple (PtFTuple pt) where
  _PtTupleP = id . _PtTupleP

data TmFTuple f a =
    TmTupleF [f a]
  | TmTupleIxF (f a) Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFTuple

instance Eq1 f => Eq1 (TmFTuple f) where
  liftEq = $(makeLiftEq ''TmFTuple)

instance Ord1 f => Ord1 (TmFTuple f) where
  liftCompare = $(makeLiftCompare ''TmFTuple)

instance Show1 f => Show1 (TmFTuple f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFTuple)

instance Bound TmFTuple where
  TmTupleF tms >>>= f = TmTupleF (fmap (>>= f) tms)
  TmTupleIxF tm i >>>= f = TmTupleIxF (tm >>= f) i

class AsTmTuple tm where
  _TmTupleP :: Prism' (tm a) (TmFTuple tm a)

  _TmTuple :: Prism' (tm a) [tm a]
  _TmTuple = _TmTupleP . _TmTupleF

  _TmTupleIx :: Prism' (tm a) (tm a, Int)
  _TmTupleIx = _TmTupleP . _TmTupleIxF

instance AsTmTuple f => AsTmTuple (TmFTuple f) where
  _TmTupleP = id . _TmTupleP

-- Errors

class AsExpectedTyTuple e ty | e -> ty where
  _ExpectedTyTuple :: Prism' e ty

expectTyTuple :: (MonadError e m, AsExpectedTyTuple e (ty a), AsTyTuple ty) => ty a -> m [ty a]
expectTyTuple ty =
  case preview _TyTuple ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyTuple ty

class AsTupleOutOfBounds e where
  _TupleOutOfBounds :: Prism' e (Int, Int)

lookupTuple :: (MonadError e m, AsTupleOutOfBounds e) =>  [t a] -> Int -> m (t a)
lookupTuple ts i =
  let
    l = length ts
    f x
      | x < 0 = throwing _TupleOutOfBounds (x, l)
      | x >= l = throwing _TupleOutOfBounds (x, l)
      | otherwise = return $ ts !! i
  in
    f i

-- Rules

stepTupleIxLazy :: AsTmTuple tm => tm a -> Maybe (tm a)
stepTupleIxLazy tm = do
  (tmT ,i) <- preview _TmTupleIx tm
  tms <- preview _TmTuple tmT
  return $ tms !! i

-- TODO check this, there might be more rules
evalRulesLazy :: AsTmTuple tm => FragmentInput e s r m ty p tm a
evalRulesLazy =
  FragmentInput [] [EvalBase stepTupleIxLazy] [] [] []

valueTuple :: AsTmTuple tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
valueTuple valueFn tm = do
  tms <- preview _TmTuple tm
  vs <- traverse valueFn tms
  return $ review _TmTuple vs

stepTupleIxStrict :: AsTmTuple tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepTupleIxStrict stepFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  tmT' <- stepFn tmT
  return $ review _TmTupleIx (tmT', i)

stepTupleElimIxStrict :: AsTmTuple tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepTupleElimIxStrict valueFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  tms <- preview _TmTuple tmT
  vs <- traverse valueFn tms
  return $ vs !! i

stepTupleIx :: AsTmTuple tm => (tm a -> Maybe (tm a)) -> (tm a -> Maybe (tm a)) -> tm a -> Int -> Maybe (tm a)
stepTupleIx valueFn stepFn tm i = do
  tms <- preview _TmTuple tm
  let (vs, s : ts) = splitAt i tms
  vs' <- traverse valueFn vs
  s' <- stepFn s
  return $ review _TmTuple (vs' ++ s' : ts)

stepTuple :: AsTmTuple tm => (tm a -> Maybe (tm a)) -> (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepTuple valueFn stepFn tm = do
  tms <- preview _TmTuple tm
  let l = length tms
  asum . fmap (stepTupleIx valueFn stepFn tm) $ [0..l-1]

evalRulesStrict :: AsTmTuple tm => FragmentInput e s r m ty p tm a
evalRulesStrict =
  FragmentInput
    [ ValueRecurse valueTuple ]
    [ EvalStep stepTupleIxStrict
    , EvalValue stepTupleElimIxStrict
    , EvalValueStep stepTuple
    ]
    [] [] []

inferTmTuple :: (Monad m, AsTyTuple ty, AsTmTuple tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmTuple inferFn tm = do
  tms <- preview _TmTuple tm
  return $ do
    tys <- traverse inferFn tms
    return $ review _TyTuple tys

inferTmTupleIx :: (MonadError e m, AsExpectedTyTuple e (ty a), AsTupleOutOfBounds e, AsTyTuple ty, AsTmTuple tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmTupleIx inferFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectTyTuple tyT
    lookupTuple tys i

inferRules :: (MonadError e m, AsExpectedTyTuple e (ty a), AsTupleOutOfBounds e, AsTyTuple ty, AsTmTuple tm) => FragmentInput e s r m ty p tm a
inferRules =
  FragmentInput
    []
    []
    [ InferRecurse inferTmTuple
    , InferRecurse inferTmTupleIx
    ]
    [] []

matchTuple :: (AsPtTuple p, AsTmTuple tm) => (p a -> tm a -> Maybe [tm a]) -> p a -> tm a -> Maybe [tm a]
matchTuple matchFn p tm = do
  pts <- preview _PtTuple p
  tms <- preview _TmTuple tm
  tmss <- zipWithM matchFn pts tms
  return $ mconcat tmss

checkTuple :: (MonadError e m, AsExpectedTyTuple e (ty a), AsTyTuple ty, AsPtTuple p) => (p a -> ty a -> m [ty a]) -> p a -> ty a -> Maybe (m [ty a])
checkTuple checkFn p ty = do
  pts <- preview _PtTuple p
  return $ do
    tys <- expectTyTuple ty
    ms <- zipWithM checkFn pts tys
    return $ mconcat ms

patternRules :: (MonadError e m, AsExpectedTyTuple e (ty a), AsTyTuple ty, AsPtTuple p, AsTmTuple tm) => FragmentInput e s r m ty p tm a
patternRules =
  FragmentInput
    [] [] [] [ PMatchRecurse matchTuple ] [ PCheckRecurse checkTuple ]

type TupleContext e s r m ty p tm a = (MonadError e m, AsExpectedTyTuple e (ty a), AsTupleOutOfBounds e, AsTyTuple ty, AsPtTuple p, AsTmTuple tm)

tupleFragmentLazy :: TupleContext e s r m ty p tm a
             => FragmentInput e s r m ty p tm a
tupleFragmentLazy =
  evalRulesLazy `mappend` inferRules `mappend` patternRules

tupleFragmentStrict :: TupleContext e s r m ty p tm a
             => FragmentInput e s r m ty p tm a
tupleFragmentStrict =
  evalRulesStrict `mappend` inferRules `mappend` patternRules

-- Helpers

tyTuple :: AsTyTuple ty => [ty a] -> ty a
tyTuple = review _TyTuple

ptTuple :: AsPtTuple pt => [pt a] -> pt a
ptTuple = review _PtTuple

tmTuple :: AsTmTuple tm => [tm a] -> tm a
tmTuple = review _TmTuple

tmTupleIx :: AsTmTuple tm => tm a -> Int -> tm a
tmTupleIx = curry $ review _TmTupleIx
