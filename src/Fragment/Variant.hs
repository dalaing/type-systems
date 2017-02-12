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
module Fragment.Variant (
    TyFVariant(..)
  , AsTyVariant(..)
  , TmFVariant(..)
  , AsTmVariant(..)
  , AsExpectedTyVariant(..)
  , AsVariantNotFound(..)
  , AsExpectedAllEq(..)
  , variantFragment
  , tyVariant
  , tmVariant
  , tmCase
  ) where

import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Fragment.Var
import Error

data TyFVariant f a =
  TyVariantF (N.NonEmpty (T.Text, f a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFVariant

deriveEq1 ''NonEmpty
deriveOrd1 ''NonEmpty
deriveShow1 ''NonEmpty

instance Eq1 f => Eq1 (TyFVariant f) where
  liftEq = $(makeLiftEq ''TyFVariant)

instance Ord1 f => Ord1 (TyFVariant f) where
  liftCompare = $(makeLiftCompare ''TyFVariant)

instance Show1 f => Show1 (TyFVariant f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFVariant)

instance Bound TyFVariant where
  TyVariantF tys >>>= f = TyVariantF (fmap (fmap (>>= f)) tys)

data TmFVariant ty tyV tm tmV =
    TmVariantF T.Text (tm tmV) (ty tyV)
  | TmCaseF (tm tmV) (Scope () tm tmV)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFVariant

instance (Eq (ty tyV), Eq1 tm, Monad tm) => Eq1 (TmFVariant ty tyV tm) where
  liftEq = $(makeLiftEq ''TmFVariant)

instance (Ord (ty tyV), Ord1 tm, Monad tm) => Ord1 (TmFVariant ty tyV tm) where
  liftCompare = $(makeLiftCompare ''TmFVariant)

instance (Show (ty tyV), Show1 tm) => Show1 (TmFVariant ty tyV tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFVariant)

instance Bound (TmFVariant ty tyV) where
  TmVariantF t tm ty >>>= f = TmVariantF t (tm >>= f) ty
  TmCaseF tm s >>>= f = TmCaseF (tm >>= f) (s >>>= f)

class AsTyVariant ty where
  _TyVariantP :: Prism' (ty a) (TyFVariant ty a)

  _TyVariant :: Prism' (ty a) (N.NonEmpty (T.Text, ty a))
  _TyVariant = _TyVariantP . _TyVariantF

instance AsTyVariant f => AsTyVariant (TyFVariant f) where
  _TyVariantP = id . _TyVariantP

class AsTmVariant ty tm | tm -> ty where
  _TmVariantP :: Prism' (tm a) (TmFVariant ty a tm a)

  _TmVariant :: Prism' (tm a) (T.Text, tm a, ty a)
  _TmVariant = _TmVariantP . _TmVariantF

  _TmCase :: Prism' (tm a) (tm a, Scope () tm a)
  _TmCase = _TmVariantP . _TmCaseF

-- Errors

class AsExpectedTyVariant e ty | e -> ty where
  _ExpectedTyVariant :: Prism' e ty

expectTyVariant :: (MonadError e m, AsExpectedTyVariant e (ty a), AsTyVariant ty) => ty a -> m (N.NonEmpty (T.Text, ty a))
expectTyVariant ty =
  case preview _TyVariant ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyVariant ty

class AsVariantNotFound e where
  _VariantNotFound :: Prism' e T.Text

lookupVariant :: (MonadError e m, AsVariantNotFound e) =>  N.NonEmpty (T.Text, t a) -> T.Text -> m (t a)
lookupVariant ts t =
  case lookup t (N.toList ts) of
    Just x -> return x
    Nothing -> throwing _VariantNotFound t

class AsExpectedAllEq e ty | e -> ty where
  _ExpectedAllEq :: Prism' e (N.NonEmpty ty)

expectAllEq :: (Eq (ty a), MonadError e m, AsExpectedAllEq e (ty a)) => N.NonEmpty (ty a) -> m (ty a)
expectAllEq (ty N.:| tys)
  | all (== ty) tys = return ty
  | otherwise = throwing _ExpectedAllEq (ty N.:| tys)

-- Rules

stepCaseVariant :: (Monad tm, AsTmVariant ty tm) => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepCaseVariant valueFn tm = do
  (tmE, s) <- preview _TmCase tm
  (_, tmV, _) <- preview _TmVariant tmE
  v <- valueFn tmV
  return $ instantiate1 v s

stepCase :: (AsTmVariant ty tm) => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepCase stepFn tm = do
  (tmE, s) <- preview _TmCase tm
  tmE' <- stepFn tmE
  return $ review _TmCase (tmE', s)

stepVariant :: (AsTmVariant ty tm) => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepVariant stepFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  tm' <- stepFn tmV
  return $ review _TmVariant (l, tm', ty)

inferTmVariant :: (Eq (ty a), MonadError e m, AsExpectedTyVariant e (ty a), AsVariantNotFound e, AsExpectedEq e (ty a), AsTyVariant ty, AsTmVariant ty tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmVariant inferFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  return $ do
    tyL <- inferFn tmV
    tys <- expectTyVariant ty
    tyV <- lookupVariant tys l
    expectEq tyL tyV
    return ty

inferTmCaseIx :: (Ord a, Monad tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a a, AsTmVar tm) => (tm a -> m (ty a)) -> Scope () tm a -> ty a -> m (ty a)
inferTmCaseIx inferFn s ty = do
  v <- freshTmVar
  let tmV = instantiate1 (review _TmVar v) s
  local (termContext %~ insertTerm v ty) $ inferFn tmV

inferTmCase :: (Ord a, Eq (ty a), Monad tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsExpectedTyVariant e (ty a), AsExpectedAllEq e (ty a), AsTyVariant ty, AsTmVar tm, AsTmVariant ty tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmCase inferFn tm = do
  (tmE, s) <- preview _TmCase tm
  return $ do
    tyE <- inferFn tmE
    vTys <- expectTyVariant tyE
    branchTys <- traverse (inferTmCaseIx inferFn s . snd) vTys
    expectAllEq branchTys

variantFragment :: (Ord a, Eq (ty a), Monad tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsExpectedTyVariant e (ty a), AsExpectedAllEq e (ty a), AsVariantNotFound e, AsExpectedEq e (ty a), AsTyVariant ty, AsTmVar tm, AsTmVariant ty tm) => FragmentInput e s r m ty p tm a
variantFragment =
  FragmentInput
    []
    [ EvalValue stepCaseVariant
    , EvalStep stepCase
    , EvalStep stepVariant
    ]
    [ InferRecurse inferTmVariant
    , InferRecurse inferTmCase
    ]
    [] []

-- Helpers

tyVariant :: AsTyVariant ty => N.NonEmpty (T.Text, ty a) -> ty a
tyVariant = review _TyVariant

tmVariant :: AsTmVariant ty tm => T.Text -> tm a -> ty a -> tm a
tmVariant l tm ty = review _TmVariant (l, tm, ty)

tmCase :: AsTmVariant ty tm => tm a -> Scope () tm a -> tm a
tmCase = curry $ review _TmCase
