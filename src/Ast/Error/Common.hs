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
  , ErrExpectedEq
  , AsExpectedEq(..)
  , ErrExpectedAllEq
  , AsExpectedAllEq(..)
  , ErrUnknownTypeError(..)
  , AsUnknownTypeError(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Data.List.NonEmpty (NonEmpty(..))

import Ast.Type
import Ast.Error

newtype ExpectedType ty a = ExpectedType (Type ty a)
  deriving (Eq, Ord, Show)

newtype ActualType ty a = ActualType (Type ty a)
  deriving (Eq, Ord, Show)

data ErrUnexpected ty a = ErrUnexpected (ExpectedType ty a) (ActualType ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnexpected

class AsUnexpected e ty a where -- | e -> ty, e -> a where
  _Unexpected :: Prism' e (ExpectedType ty a, ActualType ty a)

instance AsUnexpected (ErrUnexpected ty a) ty a where
  _Unexpected = _ErrUnexpected

instance {-# OVERLAPPABLE #-} AsUnexpected (ErrSum xs) ty a => AsUnexpected (ErrSum (x ': xs)) ty a where
  _Unexpected = _ErrNext . _Unexpected

instance {-# OVERLAPPING #-} AsUnexpected (ErrSum (ErrUnexpected ty a ': xs)) ty a where
  _Unexpected = _ErrNow . _Unexpected

data ErrExpectedEq ty a = ErrExpectedEq (Type ty a) (Type ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedEq

class AsExpectedEq e ty a where -- | e -> ty, e -> a where
  _ExpectedEq :: Prism' e (Type ty a, Type ty a)

instance AsExpectedEq (ErrExpectedEq ty a) ty a where
  _ExpectedEq = _ErrExpectedEq

instance {-# OVERLAPPABLE #-} AsExpectedEq (ErrSum xs) ty a => AsExpectedEq (ErrSum (x ': xs)) ty a where
  _ExpectedEq = _ErrNext . _ExpectedEq

instance {-# OVERLAPPING #-} AsExpectedEq (ErrSum (ErrExpectedEq ty a ': xs)) ty a where
  _ExpectedEq = _ErrNow . _ExpectedEq

data ErrExpectedAllEq ty a = ErrExpectedAllEq (NonEmpty (Type ty a))
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedAllEq

class AsExpectedAllEq e ty a where -- | e -> ty, e -> a where
  _ExpectedAllEq :: Prism' e (NonEmpty (Type ty a))

instance AsExpectedAllEq (ErrExpectedAllEq ty a) ty a where
  _ExpectedAllEq = _ErrExpectedAllEq

instance {-# OVERLAPPABLE #-} AsExpectedAllEq (ErrSum xs) ty a => AsExpectedAllEq (ErrSum (x ': xs)) ty a where
  _ExpectedAllEq = _ErrNext . _ExpectedAllEq

instance {-# OVERLAPPING #-} AsExpectedAllEq (ErrSum (ErrExpectedAllEq ty a ': xs)) ty a where
  _ExpectedAllEq = _ErrNow . _ExpectedAllEq

data ErrUnknownTypeError = ErrUnknownTypeError
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnknownTypeError

class AsUnknownTypeError e where
  _UnknownTypeError :: Prism' e ()

instance AsUnknownTypeError ErrUnknownTypeError where
  _UnknownTypeError = _ErrUnknownTypeError

instance {-# OVERLAPPABLE #-} AsUnknownTypeError (ErrSum xs) => AsUnknownTypeError (ErrSum (x ': xs)) where
  _UnknownTypeError = _ErrNext . _UnknownTypeError

instance {-# OVERLAPPING #-} AsUnknownTypeError (ErrSum (ErrUnknownTypeError ': xs)) where
  _UnknownTypeError = _ErrNow . _UnknownTypeError
