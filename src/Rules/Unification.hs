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
module Rules.Unification (
    UnificationContext
  , UnificationRule(..)
  , mapSubst
  , mkUnify
  , UConstraint(..)
  , ErrOccursError(..)
  , AsOccursError(..)
  , ErrUnificationExpectedEq(..)
  , AsUnificationExpectedEq(..)
  , ErrUnificationMismatch(..)
  , AsUnificationMismatch(..)
  ) where

import Control.Monad (when, unless, zipWithM_)
import Data.Foldable (asum, toList)
import Data.Maybe (fromMaybe)

import Control.Lens (review, preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Data.Equivalence.Monad (MonadEquiv(..), EquivT, runEquivT)

import qualified Data.Map as M
import qualified Data.Set as S

import Ast.Error

data UConstraint f a =
    UCEq (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ErrOccursError f a =
  ErrOccursError a (f a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrOccursError

class AsOccursError e f a where -- | e -> ty, e -> a where
  _OccursError :: Prism' e (a, f a)

instance AsOccursError (ErrOccursError f a) f a where
  _OccursError = _ErrOccursError

instance {-# OVERLAPPABLE #-} AsOccursError (ErrSum xs) f a => AsOccursError (ErrSum (x ': xs)) f a where
  _OccursError = _ErrNext . _OccursError

instance {-# OVERLAPPING #-} AsOccursError (ErrSum (ErrOccursError f a ': xs)) f a where
  _OccursError = _ErrNow . _OccursError

data ErrUnificationExpectedEq f a =
  ErrUnificationExpectedEq (f a) (f a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnificationExpectedEq

class AsUnificationExpectedEq e f a where -- | e -> ty, e -> a where
  _UnificationExpectedEq :: Prism' e ((f a), (f a))

instance AsUnificationExpectedEq (ErrUnificationExpectedEq f a) f a where
  _UnificationExpectedEq = _ErrUnificationExpectedEq

instance {-# OVERLAPPABLE #-} AsUnificationExpectedEq (ErrSum xs) f a => AsUnificationExpectedEq (ErrSum (x ': xs)) f a where
  _UnificationExpectedEq = _ErrNext . _UnificationExpectedEq

instance {-# OVERLAPPING #-} AsUnificationExpectedEq (ErrSum (ErrUnificationExpectedEq f a ': xs)) f a where
  _UnificationExpectedEq = _ErrNow . _UnificationExpectedEq

data ErrUnificationMismatch f a =
  ErrUnificationMismatch [f a] [f a]
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnificationMismatch

class AsUnificationMismatch e f a where -- | e -> ty, e -> a where
  _UnificationMismatch :: Prism' e ([f a], [f a])

instance AsUnificationMismatch (ErrUnificationMismatch f a) f a where
  _UnificationMismatch = _ErrUnificationMismatch

instance {-# OVERLAPPABLE #-} AsUnificationMismatch (ErrSum xs) f a => AsUnificationMismatch (ErrSum (x ': xs)) f a where
  _UnificationMismatch = _ErrNext . _UnificationMismatch

instance {-# OVERLAPPING #-} AsUnificationMismatch (ErrSum (ErrUnificationMismatch f a ': xs)) f a where
  _UnificationMismatch = _ErrNow . _UnificationMismatch

data UnificationRule m f a =
    UnificationOne (forall s. (f a -> f a -> EquivT s (f a) (f a) m ()) -> UConstraint f a -> Maybe (EquivT s (f a) (f a) m ()))
  | UnificationMany (forall s. ([f a] -> [f a] -> EquivT s (f a) (f a) m ()) -> UConstraint f a -> Maybe (EquivT s (f a) (f a) m ()))

fixUnificationRule :: (f a -> f a -> EquivT s (f a) (f a) m ())
                   -> ([f a] -> [f a] -> EquivT s (f a) (f a) m ())
                   -> UnificationRule m f a
                   -> UConstraint f a
                   -> Maybe (EquivT s (f a) (f a) m ())
fixUnificationRule oneFn _ (UnificationOne f) = f oneFn
fixUnificationRule _ manyFn (UnificationMany f) = f manyFn

mkUnifyMany :: (Eq a, Ord (f a), Foldable f, MonadError e m, AsOccursError e f a, AsUnificationMismatch e f a)
            => (UConstraint f a -> EquivT s (f a) (f a) m ())
            -> [f a]
            -> [f a]
            -> EquivT s (f a) (f a) m ()
mkUnifyMany unify1 ty1 ty2
  | length ty1 == length ty2 = zipWithM_ (\x y -> unify1 (UCEq x y)) ty1 ty2
  | otherwise = throwing _UnificationMismatch (ty1, ty2)

find :: (Eq a, Ord (f a), Monad m)
     => UConstraint f a
     -> EquivT s (f a) (f a) m (UConstraint f a)
find (UCEq ty1 ty2) = do
  c1 <- classDesc ty1
  c2 <- classDesc ty2
  return $ UCEq c1 c2

mkUnify1 :: (Eq a, Ord (f a), Foldable f, MonadError e m, AsUnificationExpectedEq e f a, AsOccursError e f a, AsUnificationMismatch e f a)
         => Prism' (f a) a
         -> (f a -> f a)
         -> [UnificationRule m f a]
         -> UConstraint f a
         -> EquivT s (f a) (f a) m ()
mkUnify1 pVar normalizeFn rules =
  let
    unifyMany = mkUnifyMany unify1
    unify1 u = do
      u' <- find u
      fromMaybe (noRuleMatches u') .
        asum .
        fmap (\r -> fixUnificationRule (\x y -> unify1 (UCEq x y)) unifyMany r u') $
        rules
    noRuleMatches (UCEq ty1 ty2) = do
      c1 <- classDesc ty1
      c2 <- classDesc ty2
      case (preview pVar c1, preview pVar c2) of
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
            throwing _UnificationExpectedEq (c1, c2)
  in
    unify1

mkUnify' :: (Eq a, Ord (f a), Foldable f, MonadError e m, AsUnificationExpectedEq e f a, AsOccursError e f a, AsUnificationMismatch e f a)
        => Prism' (f a) a
        -> (f a -> f a)
        -> [UnificationRule m f a]
        -> [UConstraint f a]
        -> EquivT s (f a) (f a) m ()
mkUnify' pVar normalizeFn rules =
  let
    unify1 = mkUnify1 pVar normalizeFn rules
    unify [] = return ()
    unify (x : xs) = do
      unify1 x
      unify xs
  in
    unify

mapSubst :: (Ord a, Monad f) => Prism' (f a) a -> M.Map a (f a) -> f a -> f a
mapSubst pVar m ty =
  ty >>= \k ->
    fromMaybe (review pVar k) .
    M.lookup k $
    m

combineMap :: (Ord a, Foldable f, Monad f) => Prism' (f a) a -> M.Map a (f a) -> M.Map a (f a) -> M.Map a (f a)
combineMap pVar m1 m2 =
  M.unionWith
    (combineType pVar)
    (fmap (mapSubst pVar m2) m1)
    (fmap (mapSubst pVar m1) m2)

combineType :: (Ord a, Foldable f) => Prism' (f a) a -> f a -> f a -> f a
combineType pVar ty1 ty2 = case (preview pVar ty1, preview pVar ty2) of
  (Just _, Just _) -> ty1
  (Just _, _) -> ty2
  (_, Just _) -> ty1
  (Nothing, Nothing) -> case compare (length ty1) (length ty2) of
    EQ -> ty1
    LT -> ty1
    GT -> ty2

mkGatherTypeSubstitution :: (Ord a, Ord (f a), Foldable f, Monad f, MonadError e m, AsUnificationExpectedEq e f a, AsOccursError e f a, AsUnificationMismatch e f a)
         => Prism' (f a) a
         -> (forall s. [UConstraint f a] -> EquivT s (f a) (f a) m ())
         -> [UConstraint f a]
         -> m (M.Map a (f a))
mkGatherTypeSubstitution pVar unify cs =
  runEquivT id (combineType pVar) $ do
    unify cs
    let vars = S.toList . S.fromList . foldMap toList $ cs
    ts <- traverse (\v -> fmap (M.singleton v) . classDesc  . review pVar $ v) vars
    return $ foldr (combineMap pVar) M.empty ts

type UnificationContext e m f a = (Ord a, Ord (f a), Foldable f, Monad f, MonadError e m, AsUnificationExpectedEq e f a, AsOccursError e f a, AsUnificationMismatch e f a)

mkUnify :: UnificationContext e m f a
        => Prism' (f a) a
        -> (f a -> f a)
        -> [UnificationRule m f a]
        -> [UConstraint f a]
        -> m (M.Map a (f a))
mkUnify pVar normalizeFn rules =
  mkGatherTypeSubstitution pVar (mkUnify' pVar normalizeFn rules)
