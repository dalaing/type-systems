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
data UConstraint ty a =
    UCEq (Type ty a) (Type ty a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''UConstraint

class AsUConstraint w ty a | w -> ty, w -> a where
  _UConstraint :: Prism' w (UConstraint ty a)

instance AsUConstraint (UConstraint ty a) ty a where
  _UConstraint = id

data UnificationRule m ty a =
  UnificationMany (forall s. ([Type ty a] -> [Type ty a] -> EquivT s (Type ty a) (Type ty a) m ()) -> Type ty a -> Type ty a -> Maybe (EquivT s (Type ty a) (Type ty a) m ()))

fixUnificationRule :: ([Type ty a] -> [Type ty a] -> EquivT s (Type ty a) (Type ty a) m ())
                   -> UnificationRule m ty a
                   -> Type ty a
                   -> Type ty a
                   -> Maybe (EquivT s (Type ty a) (Type ty a) m ())
fixUnificationRule manyFn (UnificationMany f) = f manyFn

data ErrOccursError ty a =
  ErrOccursError a (Type ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrOccursError

class AsOccursError e ty a where -- | e -> ty, e -> a where
  _OccursError :: Prism' e (a, Type ty a)

instance AsOccursError (ErrOccursError ty a) ty a where
  _OccursError = _ErrOccursError

instance {-# OVERLAPPABLE #-} AsOccursError (ErrSum xs) ty a => AsOccursError (ErrSum (x ': xs)) ty a where
  _OccursError = _ErrNext . _OccursError

instance {-# OVERLAPPING #-} AsOccursError (ErrSum (ErrOccursError ty a ': xs)) ty a where
  _OccursError = _ErrNow . _OccursError

data ErrUnificationMismatch ty a =
  ErrUnificationMismatch [Type ty a] [Type ty a]
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnificationMismatch

class AsUnificationMismatch e ty a where -- | e -> ty, e -> a where
  _UnificationMismatch :: Prism' e ([Type ty a], [Type ty a])

instance AsUnificationMismatch (ErrUnificationMismatch ty a) ty a where
  _UnificationMismatch = _ErrUnificationMismatch

instance {-# OVERLAPPABLE #-} AsUnificationMismatch (ErrSum xs) ty a => AsUnificationMismatch (ErrSum (x ': xs)) ty a where
  _UnificationMismatch = _ErrNext . _UnificationMismatch

instance {-# OVERLAPPING #-} AsUnificationMismatch (ErrSum (ErrUnificationMismatch ty a ': xs)) ty a where
  _UnificationMismatch = _ErrNow . _UnificationMismatch

mkUnifyMany :: (Ord a, OrdRec ty, Bitransversable ty, MonadError e m, AsOccursError e ty a, AsUnificationMismatch e ty a)
            => (UConstraint ty a -> EquivT s (Type ty a) (Type ty a) m ())
            -> [Type ty a]
            -> [Type ty a]
            -> EquivT s (Type ty a) (Type ty a) m ()
mkUnifyMany unify1 ty1 ty2
  | length ty1 == length ty2 = zipWithM_ (\x y -> unify1 (UCEq x y)) ty1 ty2
  | otherwise = throwing _UnificationMismatch (ty1, ty2)

mkUnify1 :: (Ord a, OrdRec ty, Bitransversable ty, MonadError e m, AsExpectedEq e ty a, AsOccursError e ty a, AsUnificationMismatch e ty a)
         => (Type ty a -> Type ty a -> Bool)
         -> [UnificationRule m ty a]
         -> UConstraint ty a
         -> EquivT s (Type ty a) (Type ty a) m ()
mkUnify1 tyEquiv rules =
  let
    unifyMany = mkUnifyMany unify1
    rule ty1 ty2 =
      asum .
      fmap (\r -> fixUnificationRule unifyMany r ty1 ty2) $
      rules
    unify1 (UCEq ty1 ty2) = do
      c1 <- classDesc ty1
      c2 <- classDesc ty2
      let
        noRuleMatches =
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
              unless (tyEquiv c1 c2) $
                throwing _ExpectedEq (c1, c2)
      fromMaybe noRuleMatches (rule c1 c2)
  in
    unify1

combineType :: (Ord a, Bitransversable ty) => Type ty a -> Type ty a -> Type ty a
combineType ty1 ty2 = case (preview _TyVar ty1, preview _TyVar ty2) of
  (Just _, Just _) -> ty1
  (Just _, _) -> ty2
  (_, Just _) -> ty1
  (Nothing, Nothing) -> case compare (length ty1) (length ty2) of
    EQ -> ty1
    LT -> ty1
    GT -> ty2

-- Does this belong in Ast.Type or Ast.Type.Var?
newtype TypeSubstitution ty a = TypeSubstitution { unTS :: M.Map a (Type ty a) }

tySubst :: (Ord a, Bound ty, Bitransversable ty) => TypeSubstitution ty a -> Type ty a -> Type ty a
tySubst (TypeSubstitution m) ty =
  ty >>= \k ->
    fromMaybe (review _TyVar k) .
    M.lookup k $
    m

instance (Ord a, Bound ty, Bitransversable ty) => Monoid (TypeSubstitution ty a) where
  mempty = TypeSubstitution mempty
  mappend ts1@(TypeSubstitution m1) ts2@(TypeSubstitution m2) =
    TypeSubstitution $ M.unionWith combineType (fmap (tySubst ts2) m1) (fmap (tySubst ts1) m2)

mkUnify' :: (Ord a, OrdRec ty, Bitransversable ty, MonadError e m, AsExpectedEq e ty a, AsOccursError e ty a, AsUnificationMismatch e ty a)
        => (Type ty a -> Type ty a -> Bool)
        -> [UnificationRule m ty a]
        -> [UConstraint ty a]
        -> EquivT s (Type ty a) (Type ty a) m ()
mkUnify' tyEquiv rules =
  let
    unify1 = mkUnify1 tyEquiv rules
    unify [] = return ()
    unify (x : xs) = do
      unify1 x
      unify xs
  in
    unify

mkGatherTypeSubstitution :: (Ord a, OrdRec ty, Bound ty, Bitransversable ty, MonadError e m, AsExpectedEq e ty a, AsOccursError e ty a, AsUnificationMismatch e ty a)
         => (forall s. [UConstraint ty a] -> EquivT s (Type ty a) (Type ty a) m ())
         -> [UConstraint ty a]
         -> m (TypeSubstitution ty a)
mkGatherTypeSubstitution unify cs =
  runEquivT id combineType $ do
    unify cs
    let vars = S.toList . S.fromList . foldMap toList $ cs
    ts <- traverse (\v -> fmap (TypeSubstitution . M.singleton v) . classDesc  . review _TyVar $ v) vars
    return $ fold ts

-- probably want a version that has a substitution map stored in a writer monad, updates on calls to unify
-- - this would be particularly useful for the online version of HM

type UnificationContext e m ty a = (Ord a, OrdRec ty, Bound ty, Bitransversable ty, MonadError e m, AsExpectedEq e ty a, AsOccursError e ty a, AsUnificationMismatch e ty a)

mkUnify :: UnificationContext e m ty a
        => (Type ty a -> Type ty a -> Bool)
        -> [UnificationRule m ty a]
        -> [UConstraint ty a]
        -> m (TypeSubstitution ty a)
mkUnify tyEquiv rules =
  mkGatherTypeSubstitution (mkUnify' tyEquiv rules)
