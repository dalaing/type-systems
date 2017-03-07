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
module Rules.Kind.Infer.SyntaxDirected (
    InferKindRule(..)
  , mkCheckKind
  , expectKind
  , expectKindEq
  , InferKindInput(..)
  , InferKindOutput(..)
  , InferKindContext
  , prepareInferKind
  ) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Data.Functor.Classes (Eq1(..))

import Ast.Kind
import Ast.Type
import Ast.Error.Common

data InferKindRule e w s r m ki ty a =
    InferKindBase (Type ki ty a -> Maybe (m (Kind ki)))
  | InferKindRecurse ((Type ki ty a -> m (Kind ki)) -> Type ki ty a -> Maybe (m (Kind ki)))

fixInferKindRule :: (Type ki ty a -> m (Kind ki))
                 -> InferKindRule e w s r m ki ty a
                 -> Type ki ty a
                 -> Maybe (m (Kind ki))
fixInferKindRule _ (InferKindBase f) = f
fixInferKindRule inferFn (InferKindRecurse f) = f inferFn

mkInferKind :: (MonadError e m, AsUnknownKindError e)
            => [InferKindRule e w s r m ki ty a]
            -> Type ki ty a -> m (Kind ki)
mkInferKind rules =
  let
    go ty =
      fromMaybe (throwing _UnknownKindError ()) .
      asum .
      fmap (\r -> fixInferKindRule go r ty) $
      rules
  in
    go

mkCheckKind :: (Eq1 ki, MonadError e m, AsUnexpectedKind e ki)
            => (Type ki ty a -> m (Kind ki))
            -> Type ki ty a
            -> Kind ki
            -> m ()
mkCheckKind inferFn =
  let
    go ty ki = do
      kiAc <- inferFn ty
      expectKind (ExpectedKind ki) (ActualKind kiAc)
  in
    go

expectKind :: (Eq1 ki, MonadError e m, AsUnexpectedKind e ki)
           => ExpectedKind ki
           -> ActualKind ki
           -> m ()
expectKind e@(ExpectedKind ki1) a@(ActualKind ki2) =
  unless (ki1 == ki2) $
    throwing _UnexpectedKind (e, a)

expectKindEq :: (Eq1 ki, MonadError e m, AsExpectedKindEq e ki)
             => Kind ki
             -> Kind ki
             -> m ()
expectKindEq ki1 ki2 =
  unless (ki1 == ki2) $
    throwing _ExpectedKindEq (ki1, ki2)

data InferKindInput e w s r m ki ty a =
  InferKindInput {
    kriInferRules :: [InferKindRule e w s r m ki ty a]
  }

instance Monoid (InferKindInput e w s r m ki ty a) where
  mempty =
    InferKindInput
      mempty
  mappend (InferKindInput kri1) (InferKindInput kri2) =
    InferKindInput
      (mappend kri1 kri2)

data InferKindOutput e w s r m ki ty a =
  InferKindOutput {
    kroInfer :: Type ki ty a -> m (Kind ki)
  , kroCheck :: Type ki ty a -> Kind ki -> m ()
  }

type InferKindContext e w s r m (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a = (Eq a, Eq1 ki, MonadError e m, AsUnexpectedKind e ki, AsUnknownKindError e)

prepareInferKind :: InferKindContext e w s r m ki ty a
                 => InferKindInput e w s r m ki ty a
                 -> InferKindOutput e w s r m ki ty a
prepareInferKind kri =
  let
    i = mkInferKind . kriInferRules $ kri
    c = mkCheckKind i
  in
    InferKindOutput i c
