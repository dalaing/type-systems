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
module Fragment.Record (
    TyFRecord(..)
  , AsTyRecord(..)
--  , PtFRecord(..)
--  , AsPtRecord(..)
  , TmFRecord(..)
  , AsTmRecord(..)
  , AsExpectedTyRecord(..)
  , AsRecordNotFound(..)
  , RecordContext
  , recordFragmentLazy
  , recordFragmentStrict
  , tyRecord
--  , ptRecord
  , tmRecord
  , tmRecordIx
  ) where

-- TODO
-- - would be good to add in-order record creation that doesn't mention the field names
-- - would also be good to have positional access to the fields
-- - the named creation should be able to shuffle the labels around, as long as everything is covered and there are no duplicates
-- - the patterns should be similar: named access in arbitrary order, or in-order anonymous access

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

class AsTyRecord ty where
  _TyRecordP :: Prism' (ty a) (TyFRecord ty a)

  _TyRecord :: Prism' (ty a) [(T.Text, ty a)]
  _TyRecord = _TyRecordP . _TyRecordF

instance AsTyRecord f => AsTyRecord (TyFRecord f) where
  _TyRecordP = id . _TyRecordP

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

valueRecord :: AsTmRecord tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
valueRecord valueFn tm = do
  tms <- preview _TmRecord tm
  vs <- traverse (traverse valueFn) tms
  return $ review _TmRecord vs

stepRecordIxStrict :: AsTmRecord tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepRecordIxStrict stepFn tm = do
  (tmR, t) <- preview _TmRecordIx tm
  tmR' <- stepFn tmR
  return $ review _TmRecordIx (tmR', t)

stepRecordElimIxStrict :: AsTmRecord tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepRecordElimIxStrict valueFn tm = do
  (tmR, t) <- preview _TmRecordIx tm
  tms <- preview _TmRecord tmR
  vs <- traverse (traverse valueFn) tms
  let Just v = lookup t vs
  return v

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
    [ ValueRecurse valueRecord ]
    [ EvalStep stepRecordIxStrict
    , EvalValue stepRecordElimIxStrict
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

type RecordContext e s r m ty (p :: * -> *) tm a = (MonadError e m, AsExpectedTyRecord e (ty a), AsRecordNotFound e, AsTyRecord ty, AsTmRecord tm)

recordFragmentLazy :: RecordContext e s r m ty p tm a
             => FragmentInput e s r m ty p tm a
recordFragmentLazy =
  mappend evalRulesLazy inferRules

recordFragmentStrict :: RecordContext e s r m ty p tm a
             => FragmentInput e s r m ty p tm a
recordFragmentStrict =
  mappend evalRulesStrict inferRules

-- Helpers

tyRecord :: AsTyRecord ty => [(T.Text, ty a)] -> ty a
tyRecord = review _TyRecord

-- ptRecord :: AsPtRecord pt => [pt a] -> pt a
-- ptRecord = review _PtRecord

tmRecord :: AsTmRecord tm => [(T.Text, tm a)] -> tm a
tmRecord = review _TmRecord

tmRecordIx :: AsTmRecord tm => tm a -> T.Text -> tm a
tmRecordIx = curry $ review _TmRecordIx
