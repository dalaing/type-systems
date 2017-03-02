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
module Rules.Type.Infer.Offline (
    InferRule(..)
  , PCheckRule(..)
  , mkCheckType
  , UnifyT
  , expectType
  , expectTypeEq
  , expectTypeAllEq
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

type UnifyT ki ty a = WriterT [UConstraint ki ty a]

data InferRule e w s r m ki ty pt tm a =
    InferBase (Term ki ty pt tm a -> Maybe (UnifyT ki ty a m (Type ki ty a)))
  | InferPCheck ((Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a)) -> (Pattern pt a -> Type ki ty a -> UnifyT ki ty a m [Type ki ty a]) -> Term ki ty pt tm a -> Maybe (UnifyT ki ty a m (Type ki ty a)))
  | InferRecurse ((Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (UnifyT ki ty a m (Type ki ty a)))

fixInferRule :: (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
             -> (Pattern pt a -> Type ki ty a -> UnifyT ki ty a m [Type ki ty a])
             -> InferRule e w s r m ki ty pt tm a
             -> Term ki ty pt tm a
             -> Maybe (UnifyT ki ty a m (Type ki ty a))
fixInferRule _ _ (InferBase f) = f
fixInferRule inferFn checkFn (InferPCheck f) = f inferFn checkFn
fixInferRule inferFn _ (InferRecurse f) = f inferFn

mkInfer' :: (MonadError e m, AsUnknownTypeError e)
        => (Type ki ty a -> Type ki ty a)
        -> (Pattern pt a -> Type ki ty a -> UnifyT ki ty a m [Type ki ty a])
        -> [InferRule e w s r m ki ty pt tm a]
        -> Term ki ty pt tm a
        -> UnifyT ki ty a m (Type ki ty a)
mkInfer' normalizeFn pc rules =
  let
    go tm =
      fmap normalizeFn .
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixInferRule go pc r tm) $
      rules
  in
    go

mkInfer :: (UnificationContext e m ki ty a, MonadError e m, AsUnknownTypeError e)
        => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
        -> ([UConstraint ki ty a] -> m (TypeSubstitution ki ty a))
        -> Term ki ty pt tm a
        -> m (Type ki ty a)
mkInfer go unifyFn x = do
  (ty, cs) <- runWriterT $ go x
  s <- unifyFn cs
  return $ tySubst s ty

mkCheckType :: (Eq a, EqRec (ty ki), Monad m)
            => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
            -> Term ki ty pt tm a
            -> Type ki ty a
            -> UnifyT ki ty a m ()
mkCheckType inferFn x y =
  let
    go tm ty = do
      tyAc <- inferFn tm
      expectType (ExpectedType ty) (ActualType tyAc)
  in
    go x y

mkCheck' :: (Eq a, EqRec (ty ki), Monad m)
        => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
        -> ([UConstraint ki ty a] -> m (TypeSubstitution ki ty a))
        -> Term ki ty pt tm a
        -> Type ki ty a
        -> m ()
mkCheck' inferFn unifyFn x y = do
  cs <- execWriterT $ (mkCheckType inferFn) x y
  _ <- unifyFn cs
  return ()

data PCheckRule e m pt ki ty a =
    PCheckBase (Pattern pt a -> Type ki ty a -> Maybe (UnifyT ki ty a m [Type ki ty a]))
  | PCheckRecurse ((Pattern pt a -> Type ki ty a -> UnifyT ki ty a m [Type ki ty a]) -> Pattern pt a -> Type ki ty a -> Maybe (UnifyT ki ty a m [Type ki ty a]))

fixPCheckRule :: (Pattern pt a -> Type ki ty a -> UnifyT ki ty a m [Type ki ty a]) -> PCheckRule e m pt ki ty a -> Pattern pt a -> Type ki ty a -> Maybe (UnifyT ki ty a m [Type ki ty a])
fixPCheckRule _ (PCheckBase f) = f
fixPCheckRule pPCheckFn (PCheckRecurse f) = f pPCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e) => [PCheckRule e m pt ki ty a] -> Pattern pt a -> Type ki ty a -> UnifyT ki ty a m [Type ki ty a]
mkPCheck rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule go r p ty) $
      rules
  in
    go x y

expectType :: (Eq a, EqRec (ty ki), Monad m) => ExpectedType ki ty a -> ActualType ki ty a -> UnifyT ki ty a m ()
expectType (ExpectedType ty1) (ActualType ty2) =
  unless (ty1 == ty2) $
    tell [review (_UConstraint . _UCEq) (ty1, ty2)]

expectTypeEq :: (Eq a, EqRec (ty ki), Monad m) => Type ki ty a -> Type ki ty a -> UnifyT ki ty a m ()
expectTypeEq ty1 ty2 =
  unless (ty1 == ty2) $
    tell [review (_UConstraint . _UCEq) (ty1, ty2)]

expectTypeAllEq :: (Eq a, EqRec (ty ki), Monad m) => NonEmpty (Type ki ty a) -> UnifyT ki ty a m (Type ki ty a)
expectTypeAllEq n@(ty :| tys) = do
  unless (all (== ty) tys ) $
    let
      xss = tails . N.toList $ n
      f [] = []
      f (x : xs) = fmap (\y -> review (_UConstraint . _UCEq) (x, y)) xs
      ws = xss >>= f
    in
      tell ws
  return ty

data InferInput e w s r m ki ty pt tm a =
  InferInput {
    iiUnifyRules :: [UnificationRule m ki ty a]
  , iiInferRules :: [InferRule e w s r m ki ty pt tm a]
  , iiPCheckRules :: [PCheckRule e m pt ki ty a]
  }

instance Monoid (InferInput e w s r m ki ty pt tm a) where
  mempty =
    InferInput mempty mempty mempty
  mappend (InferInput u1 i1 c1) (InferInput u2 i2 c2) =
    InferInput
      (mappend u1 u2)
      (mappend i1 i2)
      (mappend c1 c2)

data InferOutput e w s r m ki ty pt tm a =
  InferOutput {
    ioInfer :: Term ki ty pt tm a -> m (Type ki ty a)
  , ioCheck :: Term ki ty pt tm a -> Type ki ty a -> m ()
  }

type InferContext e w s r m (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: (* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsUnknownTypeError e, UnificationContext e m ki ty a)

prepareInfer :: InferContext e w s r m ki ty pt tm a
             => (Type ki ty a -> Type ki ty a)
             -> InferInput e w s r m ki ty pt tm a
             -> InferOutput e w s r m ki ty pt tm a
prepareInfer normalizeFn ii =
  let
    u = mkUnify normalizeFn . iiUnifyRules $ ii
    pc = mkPCheck . iiPCheckRules $ ii
    i' = mkInfer' normalizeFn pc . iiInferRules $ ii
    i = mkInfer i' u
    c = mkCheck' i' u
  in
    InferOutput i c
