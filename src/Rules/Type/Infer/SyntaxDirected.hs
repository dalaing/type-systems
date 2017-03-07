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
    InferTypeRule(..)
  , PCheckRule(..)
  , mkCheckType
  , expectType
  , expectTypeEq
  , expectTypeAllEq
  , InferTypeInput(..)
  , InferTypeOutput(..)
  , InferTypeContext
  , prepareInferType
  ) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Data.List.NonEmpty (NonEmpty(..))

import Data.Functor.Rec

import Ast.Kind
import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common

import Rules.Kind.Infer.SyntaxDirected

data InferTypeRule e w s r m ki ty pt tm a =
    InferTypeBase (Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferTypePCheck ((Term ki ty pt tm a -> m (Type ki ty a)) -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferTypeRecurse ((Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferTypeRecurseKind ((Type ki ty a -> m (Kind ki)) -> (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))

fixInferTypeRule :: (Type ki ty a -> m (Kind ki))
                 -> (Term ki ty pt tm a -> m (Type ki ty a))
                 -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a])
                 -> InferTypeRule e w s r m ki ty pt tm a
                 -> Term ki ty pt tm a
                 -> Maybe (m (Type ki ty a))
fixInferTypeRule _ _ _ (InferTypeBase f) = f
fixInferTypeRule _ inferFn checkFn (InferTypePCheck f) = f inferFn checkFn
fixInferTypeRule _ inferFn _ (InferTypeRecurse f) = f inferFn
fixInferTypeRule inferKindFn inferTypeFn _ (InferTypeRecurseKind f) = f inferKindFn inferTypeFn

mkInferType :: (MonadError e m, AsUnknownTypeError e)
            => (Type ki ty a -> m (Kind ki))
            -> (Type ki ty a -> Type ki ty a)
            -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a])
            -> [InferTypeRule e w s r m ki ty pt tm a]
            -> Term ki ty pt tm a
            -> m (Type ki ty a)
mkInferType inferKindFn normalizeFn pc rules =
  let
    go tm =
      fmap normalizeFn .
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixInferTypeRule inferKindFn go pc r tm) $
      rules
  in
    go

mkCheckType :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a)
            => (Term ki ty pt tm a -> m (Type ki ty a))
            -> Term ki ty pt tm a
            -> Type ki ty a
            -> m ()
mkCheckType inferTypeFn =
  let
    go tm ty = do
      tyAc <- inferTypeFn tm
      expectType (ExpectedType ty) (ActualType tyAc)
  in
    go

data PCheckRule e m pt ki ty a =
    PCheckBase (Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a]))
  | PCheckRecurse ((Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a]))

fixPCheckRule :: (Pattern pt a -> Type ki ty a -> m [Type ki ty a])
              -> PCheckRule e m pt ki ty a
              -> Pattern pt a
              -> Type ki ty a
              -> Maybe (m [Type ki ty a])
fixPCheckRule _ (PCheckBase f) = f
fixPCheckRule pPCheckFn (PCheckRecurse f) = f pPCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e)
         => [PCheckRule e m pt ki ty a]
         -> Pattern pt a
         -> Type ki ty a
         -> m [Type ki ty a]
mkPCheck rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule go r p ty) $
      rules
  in
    go x y

expectType :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a)
           => ExpectedType ki ty a
           -> ActualType ki ty a
           -> m ()
expectType e@(ExpectedType ty1) a@(ActualType ty2) =
  unless (ty1 == ty2) $
    throwing _UnexpectedType (e, a)

expectTypeEq :: (Eq a, EqRec (ty ki), MonadError e m, AsExpectedTypeEq e ki ty a)
             => Type ki ty a
             -> Type ki ty a
             -> m ()
expectTypeEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _ExpectedTypeEq (ty1, ty2)

expectTypeAllEq :: (Eq a, EqRec (ty ki), MonadError e m, AsExpectedTypeAllEq e ki ty a)
                => NonEmpty (Type ki ty a)
                -> m (Type ki ty a)
expectTypeAllEq (ty :| tys) = do
  unless (all (== ty) tys) $
    throwing _ExpectedTypeAllEq (ty :| tys)
  return ty

data InferTypeInput e w s r m ki ty pt tm a =
  InferTypeInput {
    iiInferRules :: [InferTypeRule e w s r m ki ty pt tm a]
  , iiPCheckRules :: [PCheckRule e m pt ki ty a]
  }

instance Monoid (InferTypeInput e w s r m ki ty pt tm a) where
  mempty =
    InferTypeInput
      mempty
      mempty
  mappend (InferTypeInput i1 c1) (InferTypeInput i2 c2) =
    InferTypeInput
      (mappend i1 i2)
      (mappend c1 c2)

data InferTypeOutput e w s r m ki ty pt tm a =
  InferTypeOutput {
    ioInfer :: Term ki ty pt tm a -> m (Type ki ty a)
  , ioCheck :: Term ki ty pt tm a -> Type ki ty a -> m ()
  }

type InferTypeContext e w s r m (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: (* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsUnknownTypeError e, InferKindContext e w s r m ki ty a)

prepareInferType :: InferTypeContext e w s r m ki ty pt tm a
             => (Type ki ty a -> m (Kind ki))
             -> (Type ki ty a -> Type ki ty a)
             -> InferTypeInput e w s r m ki ty pt tm a
             -> InferTypeOutput e w s r m ki ty pt tm a
prepareInferType inferKindFn normalizeFn ii =
  let
    i = mkInferType inferKindFn normalizeFn pc . iiInferRules $ ii
    c = mkCheckType i
    pc = mkPCheck . iiPCheckRules $ ii
  in
    InferTypeOutput i c
