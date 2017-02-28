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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Rules.Infer.Unification.Offline (
    EquivRule(..)
  , InferRule(..)
  , PCheckRule(..)
  , mkCheck
  , UnifyT
  , expect
  , expectEq
  , expectAllEq
  , InferInput(..)
  , InferOutput(..)
  , InferContext
  , prepareInfer
  ) where

import Control.Monad (unless)
import Data.List (tails)
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Lens (review)
import Control.Monad.Except (MonadError)
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT, runWriterT)
import Control.Monad.Error.Lens (throwing)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N

import Data.Functor.Rec

import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common

import Rules.Infer.Unification

data EquivRule ty a =
    EquivBase (Type ty a -> Type ty a -> Maybe Bool)
  | EquivRecurse ((Type ty a -> Type ty a -> Bool) -> Type ty a -> Type ty a -> Maybe Bool)

fixEquivRule :: (Type ty a -> Type ty a -> Bool)
             -> EquivRule ty a
             -> Type ty a
             -> Type ty a
             -> Maybe Bool
fixEquivRule _ (EquivBase f) = f
fixEquivRule equivFn (EquivRecurse f) = f equivFn

mkEquiv :: [EquivRule ty a] -> Type ty a -> Type ty a -> Bool
mkEquiv rules x y =
  let
    go ty1 ty2 =
      fromMaybe False .
      asum .
      fmap (\r -> fixEquivRule go r ty1 ty2) $
      rules
  in
    go x y

type UnifyT ty a = WriterT [UConstraint ty a]

data InferRule e w s r m ty pt tm a =
    InferBase (Term ty pt tm a -> Maybe (UnifyT ty a m (Type ty a)))
  | InferPCheck ((Term ty pt tm a -> UnifyT ty a m (Type ty a)) -> (Pattern pt a -> Type ty a -> UnifyT ty a m [Type ty a]) -> Term ty pt tm a -> Maybe (UnifyT ty a m (Type ty a)))
  | InferRecurse ((Term ty pt tm a -> UnifyT ty a m (Type ty a)) -> Term ty pt tm a -> Maybe (UnifyT ty a m (Type ty a)))

fixInferRule :: (Term ty pt tm a -> UnifyT ty a m (Type ty a))
             -> (Pattern pt a -> Type ty a -> UnifyT ty a m [Type ty a])
             -> InferRule e w s r m ty pt tm a
             -> Term ty pt tm a
             -> Maybe (UnifyT ty a m (Type ty a))
fixInferRule _ _ (InferBase f) = f
fixInferRule inferFn checkFn (InferPCheck f) = f inferFn checkFn
fixInferRule inferFn _ (InferRecurse f) = f inferFn

mkInfer' :: (MonadError e m, AsUnknownTypeError e)
        => (Pattern pt a -> Type ty a -> UnifyT ty a m [Type ty a])
        -> [InferRule e w s r m ty pt tm a]
        -> Term ty pt tm a
        -> UnifyT ty a m (Type ty a)
mkInfer' pc rules =
  let
    go tm =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixInferRule go pc r tm) $
      rules
  in
    go

mkInfer :: (UnificationContext e m ty a, MonadError e m, AsUnknownTypeError e)
        => (Term ty pt tm a -> UnifyT ty a m (Type ty a))
        -> ([UConstraint ty a] -> m (TypeSubstitution ty a))
        -> Term ty pt tm a
        -> m (Type ty a)
mkInfer go unifyFn x = do
  (ty, cs) <- runWriterT $ go x
  s <- unifyFn cs
  return $ tySubst s ty

mkCheck :: (Eq a, EqRec ty, Monad m)
        => (Term ty pt tm a -> UnifyT ty a m (Type ty a))
        -> Term ty pt tm a
        -> Type ty a
        -> UnifyT ty a m ()
mkCheck inferFn x y =
  let
    go tm ty = do
      tyAc <- inferFn tm
      expect (ExpectedType ty) (ActualType tyAc)
  in
    go x y

mkCheck' :: (Eq a, EqRec ty, Monad m)
        => (Term ty pt tm a -> UnifyT ty a m (Type ty a))
        -> ([UConstraint ty a] -> m (TypeSubstitution ty a))
        -> Term ty pt tm a
        -> Type ty a
        -> m ()
mkCheck' inferFn unifyFn x y = do
  cs <- execWriterT $ (mkCheck inferFn) x y
  _ <- unifyFn cs
  return ()

data PCheckRule e m pt ty a =
    PCheckBase (Pattern pt a -> Type ty a -> Maybe (UnifyT ty a m [Type ty a]))
  | PCheckRecurse ((Pattern pt a -> Type ty a -> UnifyT ty a m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (UnifyT ty a m [Type ty a]))

fixPCheckRule :: (Pattern pt a -> Type ty a -> UnifyT ty a m [Type ty a]) -> PCheckRule e m pt ty a -> Pattern pt a -> Type ty a -> Maybe (UnifyT ty a m [Type ty a])
fixPCheckRule _ (PCheckBase f) = f
fixPCheckRule pPCheckFn (PCheckRecurse f) = f pPCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e) => [PCheckRule e m pt ty a] -> Pattern pt a -> Type ty a -> UnifyT ty a m [Type ty a]
mkPCheck rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule go r p ty) $
      rules
  in
    go x y

expect :: (Eq a, EqRec ty, Monad m) => ExpectedType ty a -> ActualType ty a -> UnifyT ty a m ()
expect (ExpectedType ty1) (ActualType ty2) =
  unless (ty1 == ty2) $
    tell [review (_UConstraint . _UCEq) (ty1, ty2)]

expectEq :: (Eq a, EqRec ty, Monad m) => Type ty a -> Type ty a -> UnifyT ty a m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    tell [review (_UConstraint . _UCEq) (ty1, ty2)]

expectAllEq :: (Eq a, EqRec ty, Monad m) => NonEmpty (Type ty a) -> UnifyT ty a m (Type ty a)
expectAllEq n@(ty :| tys) = do
  unless (all (== ty) tys ) $
    let
      xss = tails . N.toList $ n
      f [] = []
      f (x : xs) = fmap (\y -> review (_UConstraint . _UCEq) (x, y)) xs
      ws = xss >>= f
    in
      tell ws
  return ty

data InferInput e w s r m ty pt tm a =
  InferInput {
    iiEquivRules :: [EquivRule ty a]
  , iiUnifyRules :: [UnificationRule m ty a]
  , iiInferRules :: [InferRule e w s r m ty pt tm a]
  , iiPCheckRules :: [PCheckRule e m pt ty a]
  }

instance Monoid (InferInput e w s r m ty pt tm a) where
  mempty =
    InferInput mempty mempty mempty mempty
  mappend (InferInput e1 u1 i1 c1) (InferInput e2 u2 i2 c2) =
    InferInput
      (mappend e1 e2)
      (mappend u1 u2)
      (mappend i1 i2)
      (mappend c1 c2)

data InferOutput e w s r m ty pt tm a =
  InferOutput {
    ioInfer :: Term ty pt tm a -> m (Type ty a)
  , ioCheck :: Term ty pt tm a -> Type ty a -> m ()
  }

type InferContext e w s r m (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsUnknownTypeError e, UnificationContext e m ty a)

prepareInfer :: InferContext e w s r m ty pt tm a
             => InferInput e w s r m ty pt tm a
             -> InferOutput e w s r m ty pt tm a
prepareInfer ii =
  let
    e = mkEquiv . iiEquivRules $ ii
    u = mkUnify e . iiUnifyRules $ ii
    pc = mkPCheck . iiPCheckRules $ ii
    i' = mkInfer' pc . iiInferRules $ ii
    i = mkInfer i' u
    c = mkCheck' i' u
  in
    InferOutput i c
