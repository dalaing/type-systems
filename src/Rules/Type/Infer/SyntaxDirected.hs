{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Rules.Type.Infer.SyntaxDirected (
    EquivRule(..)
  , InferRule(..)
  , PCheckRule(..)
  , mkCheckType
  , expectType
  , expectTypeEq
  , expectTypeAllEq
  , InferInput(..)
  , InferOutput(..)
  , InferContext
  , prepareInfer
  ) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Data.List.NonEmpty (NonEmpty(..))

import Data.Functor.Rec

import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common

data EquivRule ki ty a =
    EquivBase (Type ki ty a -> Type ki ty a -> Maybe Bool)
  | EquivRecurse ((Type ki ty a -> Type ki ty a -> Bool) -> Type ki ty a -> Type ki ty a -> Maybe Bool)

fixEquivRule :: (Type ki ty a -> Type ki ty a -> Bool)
             -> EquivRule ki ty a
             -> Type ki ty a
             -> Type ki ty a
             -> Maybe Bool
fixEquivRule _ (EquivBase f) = f
fixEquivRule equivFn (EquivRecurse f) = f equivFn

mkEquiv :: [EquivRule ki ty a] -> Type ki ty a -> Type ki ty a -> Bool
mkEquiv rules x y =
  let
    go ty1 ty2 =
      fromMaybe False .
      asum .
      fmap (\r -> fixEquivRule go r ty1 ty2) $
      rules
  in
    go x y

data InferRule e w s r m ki ty pt tm a =
    InferBase (Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferPCheck ((Term ki ty pt tm a -> m (Type ki ty a)) -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferTyEquivPCheck ((Type ki ty a -> Type ki ty a -> Bool) -> (Term ki ty pt tm a -> m (Type ki ty a)) -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferRecurse ((Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferTyEquivRecurse ((Type ki ty a -> Type ki ty a -> Bool) -> (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))

fixInferRule :: (Type ki ty a -> Type ki ty a -> Bool)
             -> (Term ki ty pt tm a -> m (Type ki ty a))
             -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a])
             -> InferRule e w s r m ki ty pt tm a
             -> Term ki ty pt tm a
             -> Maybe (m (Type ki ty a))
fixInferRule _ _ _ (InferBase f) = f
fixInferRule _ inferFn checkFn (InferPCheck f) = f inferFn checkFn
fixInferRule tyEquivFn inferFn checkFn (InferTyEquivPCheck f) = f tyEquivFn inferFn checkFn
fixInferRule _ inferFn _ (InferRecurse f) = f inferFn
fixInferRule tyEquivFn inferFn _ (InferTyEquivRecurse f) = f tyEquivFn inferFn

mkInfer :: (MonadError e m, AsUnknownTypeError e) => (Type ki ty a -> Type ki ty a -> Bool) -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> [InferRule e w s r m ki ty pt tm a] -> Term ki ty pt tm a -> m (Type ki ty a)
mkInfer tyEquiv pc rules =
  let
    go tm =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixInferRule tyEquiv go pc r tm) $
      rules
  in
    go

mkCheckType :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a) => (Type ki ty a -> Type ki ty a -> Bool) -> (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Type ki ty a -> m ()
mkCheckType tyEquiv inferFn =
  let
    go tm ty = do
      tyAc <- inferFn tm
      expectType tyEquiv (ExpectedType ty) (ActualType tyAc)
  in
    go

data PCheckRule e m pt ki ty a =
    PCheckBase (Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a]))
  | PCheckTyEquiv ((Type ki ty a -> Type ki ty a -> Bool) -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a]))
  | PCheckRecurse ((Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a]))

fixPCheckRule :: (Type ki ty a -> Type ki ty a -> Bool) -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> PCheckRule e m pt ki ty a -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
fixPCheckRule _ _ (PCheckBase f) = f
fixPCheckRule tyEquivFn _ (PCheckTyEquiv f) = f tyEquivFn
fixPCheckRule _ pPCheckFn (PCheckRecurse f) = f pPCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e) => (Type ki ty a -> Type ki ty a -> Bool) -> [PCheckRule e m pt ki ty a] -> Pattern pt a -> Type ki ty a -> m [Type ki ty a]
mkPCheck tyEquivFn rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule tyEquivFn go r p ty) $
      rules
  in
    go x y

expectType :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a) => (Type ki ty a -> Type ki ty a -> Bool) -> ExpectedType ki ty a -> ActualType ki ty a -> m ()
expectType tyEquiv e@(ExpectedType ty1) a@(ActualType ty2) =
  unless (ty1 `tyEquiv` ty2) $
    throwing _UnexpectedType (e, a)

expectTypeEq :: (Eq a, EqRec (ty ki), MonadError e m, AsExpectedTypeEq e ki ty a) => (Type ki ty a -> Type ki ty a -> Bool) -> Type ki ty a -> Type ki ty a -> m ()
expectTypeEq tyEquiv ty1 ty2 =
  unless (ty1 `tyEquiv` ty2) $
    throwing _ExpectedTypeEq (ty1, ty2)

expectTypeAllEq :: (Eq a, EqRec (ty ki), MonadError e m, AsExpectedTypeAllEq e ki ty a) => (Type ki ty a -> Type ki ty a -> Bool) -> NonEmpty (Type ki ty a) -> m (Type ki ty a)
expectTypeAllEq tyEquiv (ty :| tys) = do
  unless (all (tyEquiv ty) tys) $
    throwing _ExpectedTypeAllEq (ty :| tys)
  return ty

data InferInput e w s r m ki ty pt tm a =
  InferInput {
    iiEquivRules :: [EquivRule ki ty a]
  , iiInferRules :: [InferRule e w s r m ki ty pt tm a]
  , iiPCheckRules :: [PCheckRule e m pt ki ty a]
  }

instance Monoid (InferInput e w s r m ki ty pt tm a) where
  mempty =
    InferInput mempty mempty mempty
  mappend (InferInput e1 i1 c1) (InferInput e2 i2 c2) =
    InferInput
      (mappend e1 e2)
      (mappend i1 i2)
      (mappend c1 c2)

data InferOutput e w s r m ki ty pt tm a =
  InferOutput {
    ioInfer :: Term ki ty pt tm a -> m (Type ki ty a)
  , ioCheck :: Term ki ty pt tm a -> Type ki ty a -> m ()
  }

type InferContext e w s r m (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: (* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsUnknownTypeError e)

prepareInfer :: InferContext e w s r m ki ty pt tm a
             => InferInput e w s r m ki ty pt tm a
             -> InferOutput e w s r m ki ty pt tm a
prepareInfer ii =
  let
    e = mkEquiv . iiEquivRules $ ii
    i = mkInfer e pc . iiInferRules $ ii
    c = mkCheckType e i
    pc = mkPCheck e . iiPCheckRules $ ii
  in
    InferOutput i c
