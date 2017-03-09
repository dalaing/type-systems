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

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Data.List.NonEmpty (NonEmpty(..))

import Data.Functor.Rec

import Ast.Kind
import Ast.Type
import Ast.Term
import Ast.Error.Common

import Rules.Kind.Infer.SyntaxDirected

import Rules.Type.Infer.Common as X

mkCheckType :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a)
            => (Term ki ty pt tm a -> m (Type ki ty a))
            -> Term ki ty pt tm a
            -> Type ki ty a
            -> m ()
mkCheckType = mkCheckType' expectType

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

{-
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
-}

type InferTypeContext e w s r m (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: (* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsUnknownTypeError e, InferKindContext e w s r m ki ty a)

prepareInferType :: InferTypeContext e w s r m ki ty pt tm a
             => (Type ki ty a -> m (Kind ki))
             -> (Type ki ty a -> Type ki ty a)
             -> InferTypeInput e w s r m m ki ty pt tm a
             -> InferTypeOutput e w s r m ki ty pt tm a
prepareInferType inferKindFn normalizeFn ii =
  let
    i = mkInferType inferKindFn normalizeFn pc . iiInferTypeRules $ ii
    c = mkCheckType i
    pc = mkPCheck . iiPCheckRules $ ii
  in
    InferTypeOutput i c
