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
module Ast.Error.Common.Kind (
    ExpectedKind(..)
  , ActualKind(..)
  , ErrUnexpectedKind
  , AsUnexpectedKind(..)
  , ErrExpectedKindEq
  , AsExpectedKindEq(..)
  , ErrExpectedKindAllEq
  , AsExpectedKindAllEq(..)
  , ErrUnknownKindError(..)
  , AsUnknownKindError(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Data.List.NonEmpty (NonEmpty(..))

import Ast.Kind
import Ast.Error

newtype ExpectedKind ki a = ExpectedKind (Kind ki a)
  deriving (Eq, Ord, Show)

newtype ActualKind ki a = ActualKind (Kind ki a)
  deriving (Eq, Ord, Show)

data ErrUnexpectedKind ki a = ErrUnexpectedKind (ExpectedKind ki a) (ActualKind ki a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnexpectedKind

class AsUnexpectedKind e ki a where -- | e -> ty, e -> a where
  _UnexpectedKind :: Prism' e (ExpectedKind ki a, ActualKind ki a)

instance AsUnexpectedKind (ErrUnexpectedKind ki a) ki a where
  _UnexpectedKind = _ErrUnexpectedKind

instance {-# OVERLAPPABLE #-} AsUnexpectedKind (ErrSum xs) ki a => AsUnexpectedKind (ErrSum (x ': xs)) ki a where
  _UnexpectedKind = _ErrNext . _UnexpectedKind

instance {-# OVERLAPPING #-} AsUnexpectedKind (ErrSum (ErrUnexpectedKind ki a ': xs)) ki a where
  _UnexpectedKind = _ErrNow . _UnexpectedKind

data ErrExpectedKindEq ki a = ErrExpectedKindEq (Kind ki a) (Kind ki a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedKindEq

class AsExpectedKindEq e ki a where -- | e -> ty, e -> a where
  _ExpectedKindEq :: Prism' e (Kind ki a, Kind ki a)

instance AsExpectedKindEq (ErrExpectedKindEq ki a) ki a where
  _ExpectedKindEq = _ErrExpectedKindEq

instance {-# OVERLAPPABLE #-} AsExpectedKindEq (ErrSum xs) ki a => AsExpectedKindEq (ErrSum (x ': xs)) ki a where
  _ExpectedKindEq = _ErrNext . _ExpectedKindEq

instance {-# OVERLAPPING #-} AsExpectedKindEq (ErrSum (ErrExpectedKindEq ki a ': xs)) ki a where
  _ExpectedKindEq = _ErrNow . _ExpectedKindEq

data ErrExpectedKindAllEq ki a = ErrExpectedKindAllEq (NonEmpty (Kind ki a))
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedKindAllEq

class AsExpectedKindAllEq e ki a where -- | e -> ty, e -> a where
  _ExpectedKindAllEq :: Prism' e (NonEmpty (Kind ki a))

instance AsExpectedKindAllEq (ErrExpectedKindAllEq ki a) ki a where
  _ExpectedKindAllEq = _ErrExpectedKindAllEq

instance {-# OVERLAPPABLE #-} AsExpectedKindAllEq (ErrSum xs) ki a => AsExpectedKindAllEq (ErrSum (x ': xs)) ki a where
  _ExpectedKindAllEq = _ErrNext . _ExpectedKindAllEq

instance {-# OVERLAPPING #-} AsExpectedKindAllEq (ErrSum (ErrExpectedKindAllEq ki a ': xs)) ki a where
  _ExpectedKindAllEq = _ErrNow . _ExpectedKindAllEq

data ErrUnknownKindError = ErrUnknownKindError
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnknownKindError

class AsUnknownKindError e where
  _UnknownKindError :: Prism' e ()

instance AsUnknownKindError ErrUnknownKindError where
  _UnknownKindError = _ErrUnknownKindError

instance {-# OVERLAPPABLE #-} AsUnknownKindError (ErrSum xs) => AsUnknownKindError (ErrSum (x ': xs)) where
  _UnknownKindError = _ErrNext . _UnknownKindError

instance {-# OVERLAPPING #-} AsUnknownKindError (ErrSum (ErrUnknownKindError ': xs)) where
  _UnknownKindError = _ErrNow . _UnknownKindError
