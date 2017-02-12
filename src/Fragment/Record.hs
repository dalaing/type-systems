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
module Fragment.Record (
    TyFRecord(..)
  , AsTyRecord(..)
  , TmFRecord(..)
  , AsTmRecord(..)
  , AsExpectedTyRecord(..)
  , AsRecordNotFound(..)
  , recordFragmentLazy
  , recordFragmentStrict
  , tyRecord
  , tmRecord
  , tmRecordIx
  ) where

import Data.List (splitAt)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError)

import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment

data TyFRecord f a =
  TyRecordF [(T.Text, f a)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFRecord

instance Eq1 f => Eq1 (TyFRecord f) where
  liftEq = $(makeLiftEq ''TyFRecord)

instance Ord1 f => Ord1 (TyFRecord f) where
  liftCompare = $(makeLiftCompare ''TyFRecord)

instance Show1 f => Show1 (TyFRecord f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFRecord)

instance Bound TyFRecord where
  TyRecordF tys >>>= f = TyRecordF (fmap (fmap (>>= f)) tys)

-- TODO look at creation without labels (we can get them from the types)
-- TODO if we do that, add position based access as well

data TmFRecord f a =
    TmRecordF [(T.Text, f a)]
  | TmRecordIxF (f a) T.Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFRecord

instance Eq1 f => Eq1 (TmFRecord f) where
  liftEq = $(makeLiftEq ''TmFRecord)

instance Ord1 f => Ord1 (TmFRecord f) where
  liftCompare = $(makeLiftCompare ''TmFRecord)

instance Show1 f => Show1 (TmFRecord f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFRecord)

instance Bound TmFRecord where
  TmRecordF tms >>>= f = TmRecordF (fmap (fmap (>>= f)) tms)
  TmRecordIxF tm t >>>= f = TmRecordIxF (tm >>= f) t

class AsTyRecord ty where
  _TyRecordP :: Prism' (ty a) (TyFRecord ty a)

  _TyRecord :: Prism' (ty a) [(T.Text, ty a)]
  _TyRecord = _TyRecordP . _TyRecordF

instance AsTyRecord f => AsTyRecord (TyFRecord f) where
  _TyRecordP = id . _TyRecordP

class AsTmRecord tm where
  _TmRecordP :: Prism' (tm a) (TmFRecord tm a)

  _TmRecord :: Prism' (tm a) [(T.Text, tm a)]
  _TmRecord = _TmRecordP . _TmRecordF

  _TmRecordIx :: Prism' (tm a) (tm a, T.Text)
  _TmRecordIx = _TmRecordP . _TmRecordIxF

instance AsTmRecord f => AsTmRecord (TmFRecord f) where
  _TmRecordP = id . _TmRecordP

-- Errors

class AsExpectedTyRecord e ty | e -> ty where
  _ExpectedTyRecord :: Prism' e ty

expectTyRecord :: (MonadError e m, AsExpectedTyRecord e (ty a), AsTyRecord ty) => ty a -> m [(T.Text, ty a)]
expectTyRecord ty =
  case preview _TyRecord ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyRecord ty

class AsRecordNotFound e where
  _RecordNotFound :: Prism' e T.Text

lookupRecord :: (MonadError e m, AsRecordNotFound e) =>  [(T.Text, t a)] -> T.Text -> m (t a)
lookupRecord ts t =
  case lookup t ts of
    Just x -> return x
    Nothing -> throwing _RecordNotFound t

-- Rules

stepRecordIxLazy :: AsTmRecord tm => tm a -> Maybe (tm a)
stepRecordIxLazy tm = do
  (tmT, t) <- preview _TmRecordIx tm
  tms <- preview _TmRecord tmT
  let Just tm' = lookup t tms
  return tm'

-- TODO check this, there might be more rules
evalRulesLazy :: AsTmRecord tm => FragmentInput e s r m ty p tm a
evalRulesLazy =
  FragmentInput [] [EvalBase stepRecordIxLazy] [] [] []

valueRecordIx :: AsTmRecord tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
valueRecordIx valueFn tm = do
  (tmT, t) <- preview _TmRecordIx tm
  tms <- preview _TmRecord tmT
  vs <- traverse (traverse valueFn) tms
  let Just v = lookup t vs
  return v

stepRecordIxStrict :: AsTmRecord tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepRecordIxStrict stepFn tm = do
  (tmT, t) <- preview _TmRecordIx tm
  tmT' <- stepFn tmT
  return $ review _TmRecordIx (tmT', t)

stepRecordIx :: AsTmRecord tm => (tm a -> Maybe (tm a)) -> (tm a -> Maybe (tm a)) -> tm a -> Int -> Maybe (tm a)
stepRecordIx valueFn stepFn tm i = do
  tms <- preview _TmRecord tm
  let (vs, s : ts) = splitAt i tms
  vs' <- traverse (traverse valueFn) vs
  s' <- traverse stepFn s
  return $ review _TmRecord (vs' ++ s' : ts)

stepRecord :: AsTmRecord tm => (tm a -> Maybe (tm a)) -> (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepRecord valueFn stepFn tm = do
  tms <- preview _TmRecord tm
  let l = length tms
  asum . fmap (stepRecordIx valueFn stepFn tm) $ [0..l-1]

evalRulesStrict :: AsTmRecord tm => FragmentInput e s r m ty p tm a
evalRulesStrict =
  FragmentInput
    [ ValueRecurse valueRecordIx ]
    [ EvalStep stepRecordIxStrict
    , EvalValueStep stepRecord
    ]
    [] [] []

inferTmRecord :: (Monad m, AsTyRecord ty, AsTmRecord tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmRecord inferFn tm = do
  tms <- preview _TmRecord tm
  return $ do
    tys <- traverse (traverse inferFn) tms
    return $ review _TyRecord tys

inferTmRecordIx :: (MonadError e m, AsExpectedTyRecord e (ty a), AsRecordNotFound e, AsTyRecord ty, AsTmRecord tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmRecordIx inferFn tm = do
  (tmT, i) <- preview _TmRecordIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectTyRecord tyT
    lookupRecord tys i

inferRules :: (MonadError e m, AsExpectedTyRecord e (ty a), AsRecordNotFound e, AsTyRecord ty, AsTmRecord tm) => FragmentInput e s r m ty p tm a
inferRules =
  FragmentInput
    []
    []
    [ InferRecurse inferTmRecord
    , InferRecurse inferTmRecordIx
    ]
    [] []

recordFragmentLazy :: (MonadError e m, AsExpectedTyRecord e (ty a), AsRecordNotFound e, AsTyRecord ty, AsTmRecord tm)
             => FragmentInput e s r m ty p tm a
recordFragmentLazy =
  mappend evalRulesLazy inferRules

recordFragmentStrict :: (MonadError e m, AsExpectedTyRecord e (ty a), AsRecordNotFound e, AsTyRecord ty, AsTmRecord tm)
             => FragmentInput e s r m ty p tm a
recordFragmentStrict =
  mappend evalRulesStrict inferRules

-- Helpers

tyRecord :: AsTyRecord ty => [(T.Text, ty a)] -> ty a
tyRecord = review _TyRecord

tmRecord :: AsTmRecord tm => [(T.Text, tm a)] -> tm a
tmRecord = review _TmRecord

tmRecordIx :: AsTmRecord tm => tm a -> T.Text -> tm a
tmRecordIx = curry $ review _TmRecordIx
