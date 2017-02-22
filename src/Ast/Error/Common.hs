{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Ast.Error.Common (
    ExpectedType(..)
  , ActualType(..)
  , ErrUnexpected
  , AsUnexpected(..)
  , expect
  , ErrExpectedEq
  , AsExpectedEq(..)
  , expectEq
  , ErrExpectedAllEq
  , AsExpectedAllEq(..)
  , expectAllEq
  , ErrUnknownTypeError
  , AsUnknownTypeError(..)
  ) where

import Control.Monad (unless)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Data.List.NonEmpty (NonEmpty(..))

import Util

import Ast.Type
import Ast.Error

newtype ExpectedType ty a = ExpectedType (Type ty a)
  deriving (Eq, Ord, Show)

newtype ActualType ty a = ActualType (Type ty a)
  deriving (Eq, Ord, Show)

data ErrUnexpected (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrUnexpected (ExpectedType ty a) (ActualType ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnexpected

class AsUnexpected e ty a | e -> ty, e -> a where
  _Unexpected :: Prism' e (ExpectedType ty a, ActualType ty a)

instance AsUnexpected (ErrUnexpected ty pt tm a) ty a where
  _Unexpected = _ErrUnexpected

instance {-# OVERLAPPABLE #-} AsUnexpected ((ErrSum xs) ty pt tm a) ty a => AsUnexpected (ErrSum (x ': xs) ty pt tm a) ty a where
  _Unexpected = _ErrNext . _Unexpected

instance {-# OVERLAPPING #-} AsUnexpected (ErrSum (ErrUnexpected ': xs) ty pt tm a) ty a where
  _Unexpected = _ErrNow . _Unexpected

expect :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a) => ExpectedType ty a -> ActualType ty a -> m ()
expect e@(ExpectedType ty1) a@(ActualType ty2) =
  unless (ty1 == ty2) $
    throwing _Unexpected (e, a)

data ErrExpectedEq (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrExpectedEq (Type ty a) (Type ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedEq

class AsExpectedEq e ty a | e -> ty, e -> a where
  _ExpectedEq :: Prism' e (Type ty a, Type ty a)

instance AsExpectedEq (ErrExpectedEq ty pt tm a) ty a where
  _ExpectedEq = _ErrExpectedEq

instance {-# OVERLAPPABLE #-} AsExpectedEq ((ErrSum xs) ty pt tm a) ty a => AsExpectedEq (ErrSum (x ': xs) ty pt tm a) ty a where
  _ExpectedEq = _ErrNext . _ExpectedEq

instance {-# OVERLAPPING #-} AsExpectedEq (ErrSum (ErrExpectedEq ': xs) ty pt tm a) ty a where
  _ExpectedEq = _ErrNow . _ExpectedEq

expectEq :: (Eq a, EqRec ty, MonadError e m, AsExpectedEq e ty a) => Type ty a -> Type ty a -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _ExpectedEq (ty1, ty2)

data ErrExpectedAllEq (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrExpectedAllEq (NonEmpty (Type ty a))
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedAllEq

class AsExpectedAllEq e ty a | e -> ty, e -> a where
  _ExpectedAllEq :: Prism' e (NonEmpty (Type ty a))

instance AsExpectedAllEq (ErrExpectedAllEq ty pt tm a) ty a where
  _ExpectedAllEq = _ErrExpectedAllEq

instance {-# OVERLAPPABLE #-} AsExpectedAllEq ((ErrSum xs) ty pt tm a) ty a => AsExpectedAllEq (ErrSum (x ': xs) ty pt tm a) ty a where
  _ExpectedAllEq = _ErrNext . _ExpectedAllEq

instance {-# OVERLAPPING #-} AsExpectedAllEq (ErrSum (ErrExpectedAllEq ': xs) ty pt tm a) ty a where
  _ExpectedAllEq = _ErrNow . _ExpectedAllEq

expectAllEq :: (Eq a, EqRec ty, MonadError e m, AsExpectedAllEq e ty a) => NonEmpty (Type ty a) -> m (Type ty a)
expectAllEq (ty :| tys)
  | all (== ty) tys = return ty
  | otherwise = throwing _ExpectedAllEq (ty :| tys)

data ErrUnknownTypeError (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrUnknownTypeError
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnknownTypeError

class AsUnknownTypeError e where
  _UnknownTypeError :: Prism' e ()

instance AsUnknownTypeError (ErrUnknownTypeError ty pt tm a) where
  _UnknownTypeError = _ErrUnknownTypeError

instance {-# OVERLAPPABLE #-} AsUnknownTypeError ((ErrSum xs) ty pt tm a) => AsUnknownTypeError (ErrSum (x ': xs) ty pt tm a) where
  _UnknownTypeError = _ErrNext . _UnknownTypeError

instance {-# OVERLAPPING #-} AsUnknownTypeError (ErrSum (ErrUnknownTypeError ': xs) ty pt tm a) where
  _UnknownTypeError = _ErrNow . _UnknownTypeError
