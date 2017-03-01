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

newtype ExpectedType ki ty a = ExpectedType (Type ki ty a)
  deriving (Eq, Ord, Show)

newtype ActualType ki ty a = ActualType (Type ki ty a)
  deriving (Eq, Ord, Show)

data ErrUnexpected ki ty a = ErrUnexpected (ExpectedType ki ty a) (ActualType ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnexpected

class AsUnexpected e ki ty a where -- | e -> ty, e -> a where
  _Unexpected :: Prism' e (ExpectedType ki ty a, ActualType ki ty a)

instance AsUnexpected (ErrUnexpected ki ty a) ki ty a where
  _Unexpected = _ErrUnexpected

instance {-# OVERLAPPABLE #-} AsUnexpected (ErrSum xs) ki ty a => AsUnexpected (ErrSum (x ': xs)) ki ty a where
  _Unexpected = _ErrNext . _Unexpected

instance {-# OVERLAPPING #-} AsUnexpected (ErrSum (ErrUnexpected ki ty a ': xs)) ki ty a where
  _Unexpected = _ErrNow . _Unexpected

data ErrExpectedEq ki ty a = ErrExpectedEq (Type ki ty a) (Type ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedEq

class AsExpectedEq e ki ty a where -- | e -> ty, e -> a where
  _ExpectedEq :: Prism' e (Type ki ty a, Type ki ty a)

instance AsExpectedEq (ErrExpectedEq ki ty a) ki ty a where
  _ExpectedEq = _ErrExpectedEq

instance {-# OVERLAPPABLE #-} AsExpectedEq (ErrSum xs) ki ty a => AsExpectedEq (ErrSum (x ': xs)) ki ty a where
  _ExpectedEq = _ErrNext . _ExpectedEq

instance {-# OVERLAPPING #-} AsExpectedEq (ErrSum (ErrExpectedEq ki ty a ': xs)) ki ty a where
  _ExpectedEq = _ErrNow . _ExpectedEq

data ErrExpectedAllEq ki ty a = ErrExpectedAllEq (NonEmpty (Type ki ty a))
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedAllEq

class AsExpectedAllEq e ki ty a where -- | e -> ty, e -> a where
  _ExpectedAllEq :: Prism' e (NonEmpty (Type ki ty a))

instance AsExpectedAllEq (ErrExpectedAllEq ki ty a) ki ty a where
  _ExpectedAllEq = _ErrExpectedAllEq

instance {-# OVERLAPPABLE #-} AsExpectedAllEq (ErrSum xs) ki ty a => AsExpectedAllEq (ErrSum (x ': xs)) ki ty a where
  _ExpectedAllEq = _ErrNext . _ExpectedAllEq

instance {-# OVERLAPPING #-} AsExpectedAllEq (ErrSum (ErrExpectedAllEq ki ty a ': xs)) ki ty a where
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
