{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Rules.Infer (
    InferRule(..)
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

import Util

import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common

data InferRule e s r m ty pt tm a =
    InferBase (Term ty pt tm a -> Maybe (m (Type ty a)))
  | InferPCheck ((Term ty pt tm a -> m (Type ty a)) -> (Pattern pt a -> Type ty a -> m [Type ty a]) -> Term ty pt tm a -> Maybe (m (Type ty a)))
  | InferRecurse ((Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a)))

fixInferRule :: (Term ty pt tm a -> m (Type ty a))
             -> (Pattern pt a -> Type ty a -> m [Type ty a])
             -> InferRule e s r m ty pt tm a
             -> Term ty pt tm a
             -> Maybe (m (Type ty a))
fixInferRule _ _ (InferBase f) = f
fixInferRule inferFn checkFn (InferPCheck f) = f inferFn checkFn
fixInferRule inferFn _ (InferRecurse f) = f inferFn

mkInfer :: (MonadError e m, AsUnknownTypeError e) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> [InferRule e s r m ty pt tm a] -> Term ty pt tm a -> m (Type ty a)
mkInfer pc rules =
  let
    go tm =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixInferRule go pc r tm) $
      rules
  in
    go

mkCheck :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Type ty a -> m ()
mkCheck inferFn =
  let
    go tm ty = do
      tyAc <- inferFn tm
      expect (ExpectedType ty) (ActualType tyAc)
  in
    go

data PCheckRule e m pt ty a =
    PCheckBase (Pattern pt a -> Type ty a -> Maybe (m [Type ty a]))
  | PCheckRecurse ((Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a]))

fixPCheckRule :: (Pattern pt a -> Type ty a -> m [Type ty a]) -> PCheckRule e m pt ty a -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
fixPCheckRule _ (PCheckBase f) = f
fixPCheckRule pPCheckFn (PCheckRecurse f) = f pPCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e) => [PCheckRule e m pt ty a] -> Pattern pt a -> Type ty a -> m [Type ty a]
mkPCheck rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule go r p ty) $
      rules
  in
    go x y

data InferInput e s r m ty pt tm a =
  InferInput {
    iiInferRules :: [InferRule e s r m ty pt tm a]
  , iiPCheckRules :: [PCheckRule e m pt ty a]
  }

instance Monoid (InferInput e s r m ty pt tm a) where
  mempty =
    InferInput mempty mempty
  mappend (InferInput i1 c1) (InferInput i2 c2) =
    InferInput
      (mappend i1 i2)
      (mappend c1 c2)

data InferOutput e s r m ty pt tm a =
  InferOutput {
    ioInfer :: Term ty pt tm a -> m (Type ty a)
  , ioCheck :: Term ty pt tm a -> Type ty a -> m ()
  }

type InferContext e s r m (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsUnknownTypeError e)

prepareInfer :: InferContext e s r m ty pt tm a
             => InferInput e s r m ty pt tm a
             -> InferOutput e s r m ty pt tm a
prepareInfer ii =
  let
    i = mkInfer pc . iiInferRules $ ii
    c = mkCheck i
    pc = mkPCheck . iiPCheckRules $ ii
  in
    InferOutput i c
