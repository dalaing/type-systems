{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Rules.Kind.Infer.Common (
    InferKindRule(..)
  , mkInferKind
  , mkCheckKind'
  , InferKindInput(..)
  , InferKindOutput(..)

  , MonadProxy
  , BasicInferKindConstraint
  , MkInferKind(..)
  , InferKindRules(..)
  , InferKindRulesOut(..)
  , inferKindOutput
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Data.Functor.Classes

import Control.Lens.TH (makePrisms)
import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Data.List.NonEmpty (NonEmpty)

import Ast.Kind
import Ast.Error
import Ast.Error.Common.Kind
import Ast.Warning
import Ast.Type
import Data.Functor.Rec
import Rules.Unification
import Util.TypeList
import Util.MonadProxy

data InferKindRule e w s r m ki ty a =
    InferKindBase (Type ki ty a -> Maybe (m (Kind ki a)))
  | InferKindRecurse ((Type ki ty a -> m (Kind ki a)) -> Type ki ty a -> Maybe (m (Kind ki a)))

fixInferKindRule :: (Type ki ty a -> m (Kind ki a))
                 -> InferKindRule e w s r m ki ty a
                 -> Type ki ty a
                 -> Maybe (m (Kind ki a))
fixInferKindRule _ (InferKindBase f) = f
fixInferKindRule inferFn (InferKindRecurse f) = f inferFn

mkInferKind :: (MonadError e m, AsUnknownKindError e)
            => [InferKindRule e w s r m ki ty a]
            -> Type ki ty a -> m (Kind ki a)
mkInferKind rules =
  let
    go ty =
      fromMaybe (throwing _UnknownKindError ()) .
      asum .
      fmap (\r -> fixInferKindRule go r ty) $
      rules
  in
    go

mkCheckKind' :: (EqRec ki, Monad m, MkInferKind i)
             => Proxy i
             -> (ExpectedKind ki a -> ActualKind ki a  -> m ())
             -> (Type ki ty a -> m (Kind ki a))
             -> Type ki ty a
             -> Kind ki a
             -> m ()
mkCheckKind' _ expectKindFn inferFn =
  let
    go ty ki = do
      kiAc <- inferFn ty
      expectKindFn (ExpectedKind ki) (ActualKind kiAc)
  in
    go

data InferKindInput e w s r m mi ki ty a =
  InferKindInput {
    kriUnifyRules :: [UnificationRule m (Kind ki) a]
  , kriInferRules :: [InferKindRule e w s r mi ki ty a]
  }

instance Monoid (InferKindInput e w s r m mi ki ty a) where
  mempty =
    InferKindInput
      mempty
      mempty
  mappend (InferKindInput u1 i1) (InferKindInput u2 i2) =
    InferKindInput
      (mappend u1 u2)
      (mappend i1 i2)

data InferKindOutput e w s r m ki ty a =
  InferKindOutput {
    kroInfer :: Type ki ty a -> m (Kind ki a)
  , kroCheck :: Type ki ty a -> Kind ki a -> m ()
  }

class MkInferKind i where
  type MkInferKindConstraint (e :: *) (w :: *) (s :: *) (r :: *) (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i :: Constraint

  type InferKindMonad (ki :: (* -> *) -> * -> *) a (m :: * -> *) i :: (* -> *)

  type MkInferKindErrorList (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i :: [*]
  type MkInferKindWarningList (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i :: [*]

  expectKind :: MkInferKindConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy ki
             -> Proxy ty
             -> Proxy a
             -> Proxy i
             -> ExpectedKind ki a
             -> ActualKind ki a
             -> InferKindMonad ki a m i ()

  expectKindEq :: MkInferKindConstraint e w s r m ki ty a i
               => Proxy (MonadProxy e w s r m)
               -> Proxy ki
               -> Proxy ty
               -> Proxy a
               -> Proxy i
               -> Kind ki a
               -> Kind ki a
               -> InferKindMonad ki a m i ()

  expectKindAllEq :: MkInferKindConstraint e w s r m ki ty a i
                  => Proxy (MonadProxy e w s r m)
                  -> Proxy ki
                  -> Proxy ty
                  -> Proxy a
                  -> Proxy i
                  -> NonEmpty (Kind ki a)
                  -> InferKindMonad ki a m i (Kind ki a)

  mkCheckKind :: MkInferKindConstraint e w s r m ki ty a i
              => Proxy (MonadProxy e w s r m)
              -> Proxy ki
              -> Proxy ty
              -> Proxy a
              -> Proxy i
              -> (Type ki ty a -> InferKindMonad ki a m i (Kind ki a))
              -> Type ki ty a
              -> Kind ki a
              -> InferKindMonad ki a m i ()

  prepareInferKind :: MkInferKindConstraint e w s r m ki ty a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy ki
                   -> Proxy ty
                   -> Proxy a
                   -> Proxy i
                   -> InferKindInput e w s r m (InferKindMonad ki a m i) ki ty a
                   -> InferKindOutput e w s r m ki ty a

type BasicInferKindConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i =
  ( MkInferKind i
  , Monad (InferKindMonad ki a m i)
  , MkInferKindConstraint e w s r m ki ty a i
  )

class MkInferKind i => InferKindRules i (k :: j) where
  type InferKindConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i k :: Constraint

  type InferKindErrorList (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i k :: [*]
  type InferKindWarningList (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i k :: [*]

  inferKindInput :: InferKindConstraint e w s r m ki ty a i k
                 => Proxy (MonadProxy e w s r m)
                 -> Proxy i
                 -> Proxy k
                 -> InferKindInput e w s r m (InferKindMonad ki a m i) ki ty a

instance MkInferKind i => InferKindRules i '[] where
  type InferKindConstraint e w s r m ki ty a i '[] =
    MkInferKindConstraint e w s r m ki ty a i

  type InferKindErrorList ki ty a i '[] =
    Append
    (MkInferKindErrorList ki ty a i)
    ('[ ErrUnknownKindError
      , ErrUnexpectedKind ki a
      , ErrExpectedKindEq ki a
      , ErrExpectedKindAllEq ki a
      ])

  type InferKindWarningList ki ty a i '[] =
    '[]

  inferKindInput _ _ _ = mempty

instance (MkInferKind i, InferKindRules i k, InferKindRules i ks) => InferKindRules i (k ': ks) where
  type InferKindConstraint e w s r m ki ty a i (k ': ks) =
    ( InferKindConstraint e w s r m ki ty a i k
    , InferKindConstraint e w s r m ki ty a i ks
    )

  type InferKindErrorList ki ty a i (k ': ks) =
    Append
      (InferKindErrorList ki ty a i k)
      (InferKindErrorList ki ty a i ks)
  type InferKindWarningList ki ty a i (k ': ks) =
    Append
      (InferKindWarningList ki ty a i k)
      (InferKindWarningList ki ty a i ks)

  inferKindInput m i _ =
    mappend
      (inferKindInput m i (Proxy :: Proxy k))
      (inferKindInput m i (Proxy :: Proxy ks))

class InferKindRulesOut i k where
  type InferKindError (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i k :: *
  type InferKindWarning (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i k :: *

instance InferKindRules i k => InferKindRulesOut i k where
  type InferKindError ki ty a i k = ErrSum (InferKindErrorList ki ty a i k)
  type InferKindWarning ki ty a i k = WarnSum (InferKindWarningList ki ty a i k)

inferKindOutput :: (MkInferKindConstraint e w s r m ki ty a i, InferKindRules i k, InferKindConstraint e w s r m ki ty a i k)
                => Proxy (MonadProxy e w s r m)
                -> Proxy ki
                -> Proxy ty
                -> Proxy a
                -> Proxy i
                -> Proxy k
                -> InferKindOutput e w s r m ki ty a
inferKindOutput m ki ty a i k =
  prepareInferKind m ki ty a i (inferKindInput m i k)
