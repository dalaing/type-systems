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
module Rules.Infer (
    EquivRule(..)
  , InferRule(..)
  , PCheckRule(..)
  , mkCheck
  , InferInput(..)
  , InferOutput(..)
  , InferContext
  , prepareInfer
  ) where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Data.Functor.Rec

import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common

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

data InferRule e w s r m ty pt tm a =
    InferBase (Term ty pt tm a -> Maybe (m (Type ty a)))
  | InferPCheck ((Term ty pt tm a -> m (Type ty a)) -> (Pattern pt a -> Type ty a -> m [Type ty a]) -> Term ty pt tm a -> Maybe (m (Type ty a)))
  | InferTyEquivPCheck ((Type ty a -> Type ty a -> Bool) -> (Term ty pt tm a -> m (Type ty a)) -> (Pattern pt a -> Type ty a -> m [Type ty a]) -> Term ty pt tm a -> Maybe (m (Type ty a)))
  | InferRecurse ((Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a)))
  | InferTyEquivRecurse ((Type ty a -> Type ty a -> Bool) -> (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a)))

fixInferRule :: (Type ty a -> Type ty a -> Bool)
             -> (Term ty pt tm a -> m (Type ty a))
             -> (Pattern pt a -> Type ty a -> m [Type ty a])
             -> InferRule e w s r m ty pt tm a
             -> Term ty pt tm a
             -> Maybe (m (Type ty a))
fixInferRule _ _ _ (InferBase f) = f
fixInferRule _ inferFn checkFn (InferPCheck f) = f inferFn checkFn
fixInferRule tyEquivFn inferFn checkFn (InferTyEquivPCheck f) = f tyEquivFn inferFn checkFn
fixInferRule _ inferFn _ (InferRecurse f) = f inferFn
fixInferRule tyEquivFn inferFn _ (InferTyEquivRecurse f) = f tyEquivFn inferFn

mkInfer :: (MonadError e m, AsUnknownTypeError e) => (Type ty a -> Type ty a -> Bool) -> (Pattern pt a -> Type ty a -> m [Type ty a]) -> [InferRule e w s r m ty pt tm a] -> Term ty pt tm a -> m (Type ty a)
mkInfer tyEquiv pc rules =
  let
    go tm =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixInferRule tyEquiv go pc r tm) $
      rules
  in
    go

mkCheck :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a) => (Type ty a -> Type ty a -> Bool) -> (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Type ty a -> m ()
mkCheck tyEquiv inferFn =
  let
    go tm ty = do
      tyAc <- inferFn tm
      expect tyEquiv (ExpectedType ty) (ActualType tyAc)
  in
    go

data PCheckRule e m pt ty a =
    PCheckBase (Pattern pt a -> Type ty a -> Maybe (m [Type ty a]))
  | PCheckTyEquiv ((Type ty a -> Type ty a -> Bool) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a]))
  | PCheckRecurse ((Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a]))

fixPCheckRule :: (Type ty a -> Type ty a -> Bool) -> (Pattern pt a -> Type ty a -> m [Type ty a]) -> PCheckRule e m pt ty a -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
fixPCheckRule _ _ (PCheckBase f) = f
fixPCheckRule tyEquivFn _ (PCheckTyEquiv f) = f tyEquivFn
fixPCheckRule _ pPCheckFn (PCheckRecurse f) = f pPCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e) => (Type ty a -> Type ty a -> Bool) -> [PCheckRule e m pt ty a] -> Pattern pt a -> Type ty a -> m [Type ty a]
mkPCheck tyEquivFn rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule tyEquivFn go r p ty) $
      rules
  in
    go x y

data InferInput e w s r m ty pt tm a =
  InferInput {
    iiEquivRules :: [EquivRule ty a]
  , iiInferRules :: [InferRule e w s r m ty pt tm a]
  , iiPCheckRules :: [PCheckRule e m pt ty a]
  }

instance Monoid (InferInput e w s r m ty pt tm a) where
  mempty =
    InferInput mempty mempty mempty
  mappend (InferInput e1 i1 c1) (InferInput e2 i2 c2) =
    InferInput
      (mappend e1 e2)
      (mappend i1 i2)
      (mappend c1 c2)

data InferOutput e w s r m ty pt tm a =
  InferOutput {
    ioInfer :: Term ty pt tm a -> m (Type ty a)
  , ioCheck :: Term ty pt tm a -> Type ty a -> m ()
  }

type InferContext e w s r m (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsUnknownTypeError e)

prepareInfer :: InferContext e w s r m ty pt tm a
             => InferInput e w s r m ty pt tm a
             -> InferOutput e w s r m ty pt tm a
prepareInfer ii =
  let
    e = mkEquiv . iiEquivRules $ ii
    i = mkInfer e pc . iiInferRules $ ii
    c = mkCheck e i
    pc = mkPCheck e . iiPCheckRules $ ii
  in
    InferOutput i c
