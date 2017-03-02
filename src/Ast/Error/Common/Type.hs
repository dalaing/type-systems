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
module Ast.Error.Common.Type (
    ExpectedType(..)
  , ActualType(..)
  , ErrUnexpectedType
  , AsUnexpectedType(..)
  , ErrExpectedTypeEq
  , AsExpectedTypeEq(..)
  , ErrExpectedTypeAllEq
  , AsExpectedTypeAllEq(..)
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

data ErrUnexpectedType ki ty a = ErrUnexpectedType (ExpectedType ki ty a) (ActualType ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnexpectedType

class AsUnexpectedType e ki ty a where -- | e -> ty, e -> a where
  _UnexpectedType :: Prism' e (ExpectedType ki ty a, ActualType ki ty a)

instance AsUnexpectedType (ErrUnexpectedType ki ty a) ki ty a where
  _UnexpectedType = _ErrUnexpectedType

instance {-# OVERLAPPABLE #-} AsUnexpectedType (ErrSum xs) ki ty a => AsUnexpectedType (ErrSum (x ': xs)) ki ty a where
  _UnexpectedType = _ErrNext . _UnexpectedType

instance {-# OVERLAPPING #-} AsUnexpectedType (ErrSum (ErrUnexpectedType ki ty a ': xs)) ki ty a where
  _UnexpectedType = _ErrNow . _UnexpectedType

data ErrExpectedTypeEq ki ty a = ErrExpectedTypeEq (Type ki ty a) (Type ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTypeEq

class AsExpectedTypeEq e ki ty a where -- | e -> ty, e -> a where
  _ExpectedTypeEq :: Prism' e (Type ki ty a, Type ki ty a)

instance AsExpectedTypeEq (ErrExpectedTypeEq ki ty a) ki ty a where
  _ExpectedTypeEq = _ErrExpectedTypeEq

instance {-# OVERLAPPABLE #-} AsExpectedTypeEq (ErrSum xs) ki ty a => AsExpectedTypeEq (ErrSum (x ': xs)) ki ty a where
  _ExpectedTypeEq = _ErrNext . _ExpectedTypeEq

instance {-# OVERLAPPING #-} AsExpectedTypeEq (ErrSum (ErrExpectedTypeEq ki ty a ': xs)) ki ty a where
  _ExpectedTypeEq = _ErrNow . _ExpectedTypeEq

data ErrExpectedTypeAllEq ki ty a = ErrExpectedTypeAllEq (NonEmpty (Type ki ty a))
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTypeAllEq

class AsExpectedTypeAllEq e ki ty a where -- | e -> ty, e -> a where
  _ExpectedTypeAllEq :: Prism' e (NonEmpty (Type ki ty a))

instance AsExpectedTypeAllEq (ErrExpectedTypeAllEq ki ty a) ki ty a where
  _ExpectedTypeAllEq = _ErrExpectedTypeAllEq

instance {-# OVERLAPPABLE #-} AsExpectedTypeAllEq (ErrSum xs) ki ty a => AsExpectedTypeAllEq (ErrSum (x ': xs)) ki ty a where
  _ExpectedTypeAllEq = _ErrNext . _ExpectedTypeAllEq

instance {-# OVERLAPPING #-} AsExpectedTypeAllEq (ErrSum (ErrExpectedTypeAllEq ki ty a ': xs)) ki ty a where
  _ExpectedTypeAllEq = _ErrNow . _ExpectedTypeAllEq

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

