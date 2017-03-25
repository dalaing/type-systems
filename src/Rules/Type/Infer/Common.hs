{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Rules.Type.Infer.Common (
    InferTypeRule(..)
  , mkInferType
  , mkCheckType'
  , PCheckRule(..)
  , mkPCheck
  , InferTypeInput(..)
  , InferTypeOutput(..)

  , MonadProxy
  , BasicInferTypeConstraint
  , MkInferType(..)
  , InferTypeRules(..)
  , InferTypeRulesOut(..)
  , inferTypeOutput
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Data.List.NonEmpty (NonEmpty)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import qualified Data.Map as M

import Ast.Kind
import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error
import Ast.Error.Common
import Ast.Warning
import Data.Functor.Rec
import Util.TypeList

import Rules.Unification

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

mkCheckType' :: (Eq a, EqRec (ty ki), Monad m)
            => (ExpectedType ki ty a -> ActualType ki ty a -> m ())
            -> (Term ki ty pt tm a -> m (Type ki ty a))
            -> Term ki ty pt tm a
            -> Type ki ty a
            -> m ()
mkCheckType' expectTypeFn inferTypeFn =
  let
    go tm ty = do
      tyAc <- inferTypeFn tm
      expectTypeFn (ExpectedType ty) (ActualType tyAc)
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

data InferTypeInput e w s r m mi ki ty pt tm a =
  InferTypeInput {
    iiUnifyRules :: [UnificationRule m (Type ki ty) a]
  , iiInferTypeRules :: [InferTypeRule e w s r mi ki ty pt tm a]
  , iiPCheckRules :: [PCheckRule e mi pt ki ty a]
  }

instance Monoid (InferTypeInput e w s r m mi ki ty pt tm a) where
  mempty =
    InferTypeInput mempty mempty mempty
  mappend (InferTypeInput u1 i1 c1) (InferTypeInput u2 i2 c2) =
    InferTypeInput
      (mappend u1 u2)
      (mappend i1 i2)
      (mappend c1 c2)

data InferTypeOutput e w s r m ki ty pt tm a =
  InferTypeOutput {
    ioUnify :: [UConstraint (Type ki ty) a] -> m (M.Map a (Type ki ty a))
  , ioInfer :: Term ki ty pt tm a -> m (Type ki ty a)
  , ioCheck :: Term ki ty pt tm a -> Type ki ty a -> m ()
  }

data MonadProxy (e :: *) (w :: *) (s :: *) (r :: *) (m :: * -> *)

class MkInferType i where
  type MkInferTypeConstraint (e :: *) (w :: *) (s :: *) (r :: *) (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a i :: Constraint
  type InferTypeMonad (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a (m :: * -> *) i :: (* -> *)

  type MkInferErrorList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a i :: [*]
  type MkInferWarningList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a i :: [*]

  expectType :: MkInferTypeConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> ExpectedType ki ty a
             -> ActualType ki ty a
             -> InferTypeMonad ki ty a m i ()

  expectTypeEq :: MkInferTypeConstraint e w s r m ki ty a i
               => Proxy (MonadProxy e w s r m)
               -> Proxy i
               -> Type ki ty a
               -> Type ki ty a
               -> InferTypeMonad ki ty a m i ()

  expectTypeAllEq :: MkInferTypeConstraint e w s r m ki ty a i
                  => Proxy (MonadProxy e w s r m)
                  -> Proxy i
                  -> NonEmpty (Type ki ty a)
                  -> InferTypeMonad ki ty a m i (Type ki ty a)

  mkCheckType :: MkInferTypeConstraint e w s r m ki ty a i
              => Proxy (MonadProxy e w s r m)
              -> Proxy i
              -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
              -> Term ki ty pt tm a
              -> Type ki ty a
              -> InferTypeMonad ki ty a m i ()

  prepareInferType :: MkInferTypeConstraint e w s r m ki ty a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> (Type ki ty a -> InferTypeMonad ki ty a m i (Kind ki))
                   -> (Type ki ty a -> Type ki ty a)
                   -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
                   -> InferTypeOutput e w s r m ki ty pt tm a

type BasicInferTypeConstraint e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a i = ( MkInferType i
      , Monad (InferTypeMonad ki ty a m i)
      , MkInferTypeConstraint e w s r m ki ty a i
      )

class MkInferType i => InferTypeRules i (k :: j) where
  type InferTypeConstraint e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a i k :: Constraint

  type ErrorList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a i k :: [*]
  type WarningList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a i k :: [*]

  inferTypeInput :: InferTypeConstraint e w s r m ki ty pt tm a i k
                 => Proxy (MonadProxy e w s r m)
                 -> Proxy i
                 -> Proxy k
                 -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a

instance MkInferType i => InferTypeRules i '[] where
  type InferTypeConstraint e w s r m ki ty pt tm a i '[] =
    MkInferTypeConstraint e w s r m ki ty a i

  -- pull in error lists from MkInferType i
  type ErrorList ki ty pt tm a i '[] =
    Append
    (MkInferErrorList ki ty pt tm a i)
    ('[ ErrUnknownTypeError
      , ErrUnexpectedType ki ty a
      , ErrExpectedTypeEq ki ty a
      , ErrExpectedTypeAllEq ki ty a
      ])

  type WarningList ki ty pt tm a i '[] =
    '[]

  inferTypeInput _ _ _ = mempty

instance (MkInferType i, InferTypeRules i k, InferTypeRules i ks) => InferTypeRules i (k ': ks) where
  type InferTypeConstraint e w s r m ki ty pt tm a i (k ': ks) =
    ( InferTypeConstraint e w s r m ki ty pt tm a i k
    , InferTypeConstraint e w s r m ki ty pt tm a i ks
    )

  type ErrorList ki ty pt tm a i (k ': ks) =
    Append (ErrorList ki ty pt tm a i k) (ErrorList ki ty pt tm a i ks)
  type WarningList ki ty pt tm a i (k ': ks) =
    Append (WarningList ki ty pt tm a i k) (WarningList ki ty pt tm a i ks)

  inferTypeInput m i _ =
    mappend
      (inferTypeInput m i (Proxy :: Proxy k))
      (inferTypeInput m i (Proxy :: Proxy ks))

class InferTypeRulesOut i k where
  type RError (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a i k :: *
  type RWarning (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a i k :: *

instance InferTypeRules i k => InferTypeRulesOut i k where
  type RError ki ty pt tm a i k = ErrSum (ErrorList ki ty pt tm a i k)
  type RWarning ki ty pt tm a i k = WarnSum (WarningList ki ty pt tm a i k)

inferTypeOutput :: (MkInferTypeConstraint e w s r m ki ty a i, InferTypeRules i k, InferTypeConstraint e w s r m ki ty pt tm a i k)
                => Proxy (MonadProxy e w s r m)
                -> Proxy i
                -> Proxy k
                -> (Type ki ty a -> InferTypeMonad ki ty a m i (Kind ki))
                -> (Type ki ty a -> Type ki ty a)
                -> InferTypeOutput e w s r m ki ty pt tm a
inferTypeOutput m i k inferKindFn normalizeTypeFn =
  prepareInferType m i inferKindFn normalizeTypeFn (inferTypeInput m i k)
