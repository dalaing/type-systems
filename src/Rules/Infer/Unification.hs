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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Rules.Infer.Unification (
    UnificationRule(..)
  , UnificationContext
  , mkUnify
  , UConstraint(..)
  , _UCEq
  , AsUConstraint(..)
  , TypeSubstitution(..)
  , tySubst
  , ErrOccursError(..)
  , AsOccursError(..)
  , ErrUnificationMismatch(..)
  , AsUnificationMismatch(..)
  ) where

import Control.Monad (when, unless, zipWithM_)
import Data.Foldable (asum, toList, fold)
import Data.Maybe (fromMaybe)

import Bound (Bound)
import Control.Lens (review, preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Data.Equivalence.Monad (MonadEquiv(..), EquivT, runEquivT)

import qualified Data.Map as M
import qualified Data.Set as S

import Ast.Type
import Ast.Error
import Ast.Error.Common
import Data.Bitransversable
import Data.Functor.Rec

-- need to blow this apart once we want optional type classes
-- or do we just go for a UniSum, with a base list including UCEq?
-- maybe wait until typeclasses and or HML
-- - constraint resolution will get interesting with typeclasses
-- - our general machinery will get more involved with HML and friends
data UConstraint ki ty a =
    UCEq (Type ki ty a) (Type ki ty a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''UConstraint

class AsUConstraint w ki ty a | w -> ty, w -> a where
  _UConstraint :: Prism' w (UConstraint ki ty a)

instance AsUConstraint (UConstraint ki ty a) ki ty a where
  _UConstraint = id

data ErrOccursError ki ty a =
  ErrOccursError a (Type ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrOccursError

class AsOccursError e ki ty a where -- | e -> ty, e -> a where
  _OccursError :: Prism' e (a, Type ki ty a)

instance AsOccursError (ErrOccursError ki ty a) ki ty a where
  _OccursError = _ErrOccursError

instance {-# OVERLAPPABLE #-} AsOccursError (ErrSum xs) ki ty a => AsOccursError (ErrSum (x ': xs)) ki ty a where
  _OccursError = _ErrNext . _OccursError

instance {-# OVERLAPPING #-} AsOccursError (ErrSum (ErrOccursError ki ty a ': xs)) ki ty a where
  _OccursError = _ErrNow . _OccursError

data ErrUnificationMismatch ki ty a =
  ErrUnificationMismatch [Type ki ty a] [Type ki ty a]
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnificationMismatch

class AsUnificationMismatch e ki ty a where -- | e -> ty, e -> a where
  _UnificationMismatch :: Prism' e ([Type ki ty a], [Type ki ty a])

instance AsUnificationMismatch (ErrUnificationMismatch ki ty a) ki ty a where
  _UnificationMismatch = _ErrUnificationMismatch

instance {-# OVERLAPPABLE #-} AsUnificationMismatch (ErrSum xs) ki ty a => AsUnificationMismatch (ErrSum (x ': xs)) ki ty a where
  _UnificationMismatch = _ErrNext . _UnificationMismatch

instance {-# OVERLAPPING #-} AsUnificationMismatch (ErrSum (ErrUnificationMismatch ki ty a ': xs)) ki ty a where
  _UnificationMismatch = _ErrNow . _UnificationMismatch

data UnificationRule m ki ty a =
    UnificationOne (forall s. (Type ki ty a -> Type ki ty a -> EquivT s (Type ki ty a) (Type ki ty a) m ()) -> UConstraint ki ty a -> Maybe (EquivT s (Type ki ty a) (Type ki ty a) m ()))
  | UnificationMany (forall s. ([Type ki ty a] -> [Type ki ty a] -> EquivT s (Type ki ty a) (Type ki ty a) m ()) -> UConstraint ki ty a -> Maybe (EquivT s (Type ki ty a) (Type ki ty a) m ()))

fixUnificationRule :: (Type ki ty a -> Type ki ty a -> EquivT s (Type ki ty a) (Type ki ty a) m ())
                   -> ([Type ki ty a] -> [Type ki ty a] -> EquivT s (Type ki ty a) (Type ki ty a) m ())
                   -> UnificationRule m ki ty a
                   -> UConstraint ki ty a
                   -> Maybe (EquivT s (Type ki ty a) (Type ki ty a) m ())
fixUnificationRule oneFn _ (UnificationOne f) = f oneFn
fixUnificationRule _ manyFn (UnificationMany f) = f manyFn

mkUnifyMany :: (Ord a, OrdRec (ty ki), Bitransversable (ty ki), MonadError e m, AsOccursError e ki ty a, AsUnificationMismatch e ki ty a)
            => (UConstraint ki ty a -> EquivT s (Type ki ty a) (Type ki ty a) m ())
            -> [Type ki ty a]
            -> [Type ki ty a]
            -> EquivT s (Type ki ty a) (Type ki ty a) m ()
mkUnifyMany unify1 ty1 ty2
  | length ty1 == length ty2 = zipWithM_ (\x y -> unify1 (UCEq x y)) ty1 ty2
  | otherwise = throwing _UnificationMismatch (ty1, ty2)

mkUnify1 :: (Ord a, OrdRec (ty ki), Bitransversable (ty ki), MonadError e m, AsExpectedTypeEq e ki ty a, AsOccursError e ki ty a, AsUnificationMismatch e ki ty a)
         => (Type ki ty a -> Type ki ty a)
         -> [UnificationRule m ki ty a]
         -> UConstraint ki ty a
         -> EquivT s (Type ki ty a) (Type ki ty a) m ()
mkUnify1 normalizeFn rules =
  let
    unifyMany = mkUnifyMany unify1
    unify1 u =
      fromMaybe (noRuleMatches u) .
      asum .
      fmap (\r -> fixUnificationRule (\x y -> unify1 (UCEq x y)) unifyMany r u) $
      rules
    noRuleMatches (UCEq ty1 ty2) = do
      c1 <- classDesc ty1
      c2 <- classDesc ty2
      case (preview _TyVar c1, preview _TyVar c2) of
        (Just v1, _) -> do
          when (v1 `elem` c2) $
            throwing _OccursError (v1, c2)
          equate c1 c2
          return ()
        (_, Just v2) -> do
          when (v2 `elem` c1) $
            throwing _OccursError (v2, c1)
          equate c1 c2
        (Nothing, Nothing) ->
          unless (normalizeFn c1 == normalizeFn c2) $
            throwing _ExpectedTypeEq (c1, c2)
  in
    unify1

mkUnify' :: (Ord a, OrdRec (ty ki), Bitransversable (ty ki), MonadError e m, AsExpectedTypeEq e ki ty a, AsOccursError e ki ty a, AsUnificationMismatch e ki ty a)
        => (Type ki ty a -> Type ki ty a)
        -> [UnificationRule m ki ty a]
        -> [UConstraint ki ty a]
        -> EquivT s (Type ki ty a) (Type ki ty a) m ()
mkUnify' normalizeFn rules =
  let
    unify1 = mkUnify1 normalizeFn rules
    unify [] = return ()
    unify (x : xs) = do
      unify1 x
      unify xs
  in
    unify

combineType :: (Ord a, Bitransversable (ty ki)) => Type ki ty a -> Type ki ty a -> Type ki ty a
combineType ty1 ty2 = case (preview _TyVar ty1, preview _TyVar ty2) of
  (Just _, Just _) -> ty1
  (Just _, _) -> ty2
  (_, Just _) -> ty1
  (Nothing, Nothing) -> case compare (length ty1) (length ty2) of
    EQ -> ty1
    LT -> ty1
    GT -> ty2

-- Does this belong in Ast.Type or Ast.Type.Var?
newtype TypeSubstitution ki ty a = TypeSubstitution { unTS :: M.Map a (Type ki ty a) }

tySubst :: (Ord a, Bound (ty ki), Bitransversable (ty ki)) => TypeSubstitution ki ty a -> Type ki ty a -> Type ki ty a
tySubst (TypeSubstitution m) ty =
  ty >>= \k ->
    fromMaybe (review _TyVar k) .
    M.lookup k $
    m

instance (Ord a, Bound (ty ki), Bitransversable (ty ki)) => Monoid (TypeSubstitution ki ty a) where
  mempty = TypeSubstitution mempty
  mappend ts1@(TypeSubstitution m1) ts2@(TypeSubstitution m2) =
    TypeSubstitution $ M.unionWith combineType (fmap (tySubst ts2) m1) (fmap (tySubst ts1) m2)

mkGatherTypeSubstitution :: (Ord a, OrdRec (ty ki), Bound (ty ki), Bitransversable (ty ki), MonadError e m, AsExpectedTypeEq e ki ty a, AsOccursError e ki ty a, AsUnificationMismatch e ki ty a)
         => (forall s. [UConstraint ki ty a] -> EquivT s (Type ki ty a) (Type ki ty a) m ())
         -> [UConstraint ki ty a]
         -> m (TypeSubstitution ki ty a)
mkGatherTypeSubstitution unify cs =
  runEquivT id combineType $ do
    unify cs
    let vars = S.toList . S.fromList . foldMap toList $ cs
    ts <- traverse (\v -> fmap (TypeSubstitution . M.singleton v) . classDesc  . review _TyVar $ v) vars
    return $ fold ts

-- probably want a version that has a substitution map stored in a writer monad, updates on calls to unify
-- - this would be particularly useful for the online version of HM

type UnificationContext e m ki ty a = (Ord a, OrdRec (ty ki), Bound (ty ki), Bitransversable (ty ki), MonadError e m, AsExpectedTypeEq e ki ty a, AsOccursError e ki ty a, AsUnificationMismatch e ki ty a)

mkUnify :: UnificationContext e m ki ty a
        => (Type ki ty a -> Type ki ty a)
        -> [UnificationRule m ki ty a]
        -> [UConstraint ki ty a]
        -> m (TypeSubstitution ki ty a)
mkUnify normalizeFn rules =
  mkGatherTypeSubstitution (mkUnify' normalizeFn rules)
