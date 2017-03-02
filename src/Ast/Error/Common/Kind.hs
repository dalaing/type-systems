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
  , ErrUnknownKindError(..)
  , AsUnknownKindError(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Kind
import Ast.Error

newtype ExpectedKind ki = ExpectedKind (Kind ki)
  deriving (Eq, Ord, Show)

newtype ActualKind ki = ActualKind (Kind ki)
  deriving (Eq, Ord, Show)

data ErrUnexpectedKind ki = ErrUnexpectedKind (ExpectedKind ki ) (ActualKind ki)
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnexpectedKind

class AsUnexpectedKind e ki where -- | e -> ty, e -> a where
  _UnexpectedKind :: Prism' e (ExpectedKind ki, ActualKind ki)

instance AsUnexpectedKind (ErrUnexpectedKind ki) ki where
  _UnexpectedKind = _ErrUnexpectedKind

instance {-# OVERLAPPABLE #-} AsUnexpectedKind (ErrSum xs) ki => AsUnexpectedKind (ErrSum (x ': xs)) ki where
  _UnexpectedKind = _ErrNext . _UnexpectedKind

instance {-# OVERLAPPING #-} AsUnexpectedKind (ErrSum (ErrUnexpectedKind ki ': xs)) ki where
  _UnexpectedKind = _ErrNow . _UnexpectedKind

data ErrExpectedKindEq ki = ErrExpectedKindEq (Kind ki) (Kind ki)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedKindEq

class AsExpectedKindEq e ki where -- | e -> ty, e -> a where
  _ExpectedKindEq :: Prism' e (Kind ki, Kind ki)

instance AsExpectedKindEq (ErrExpectedKindEq ki) ki where
  _ExpectedKindEq = _ErrExpectedKindEq

instance {-# OVERLAPPABLE #-} AsExpectedKindEq (ErrSum xs) ki => AsExpectedKindEq (ErrSum (x ': xs)) ki where
  _ExpectedKindEq = _ErrNext . _ExpectedKindEq

instance {-# OVERLAPPING #-} AsExpectedKindEq (ErrSum (ErrExpectedKindEq ki ': xs)) ki where
  _ExpectedKindEq = _ErrNow . _ExpectedKindEq

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
