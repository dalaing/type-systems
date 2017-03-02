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
module Fragment.SystemFw.Ast.Error (
    ErrExpectedTyArr(..)
  , AsExpectedTyArr(..)
  , expectTyArr
  , ErrExpectedTyAll(..)
  , AsExpectedTyAll(..)
  , expectTyAll
  ) where

import Bound (Scope)
import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens (preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Kind
import Ast.Type
import Ast.Error

import Fragment.SystemFw.Ast.Type

data ErrExpectedTyArr ki ty a = ErrExpectedTyArr (Type ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyArr

class AsExpectedTyArr e ki ty a where -- | e -> ty, e -> a where
  _ExpectedTyArr :: Prism' e (Type ki ty a)

instance AsExpectedTyArr (ErrExpectedTyArr ki ty a) ki ty a where
  _ExpectedTyArr = _ErrExpectedTyArr

instance {-# OVERLAPPABLE #-} AsExpectedTyArr (ErrSum xs) ki ty a => AsExpectedTyArr (ErrSum (x ': xs)) ki ty a where
  _ExpectedTyArr = _ErrNext . _ExpectedTyArr

instance {-# OVERLAPPING #-} AsExpectedTyArr (ErrSum (ErrExpectedTyArr ki ty a ': xs)) ki ty a where
  _ExpectedTyArr = _ErrNow . _ExpectedTyArr

expectTyArr :: (MonadError e m, AsExpectedTyArr e ki ty a, AsTySystemFw ki ty) => Type ki ty a -> m (Type ki ty a, Type ki ty a)
expectTyArr ty =
  case preview _TyArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedTyArr ty

data ErrExpectedTyAll ki ty a = ErrExpectedTyAll (Type ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyAll

class AsExpectedTyAll e ki ty a where -- | e -> ty, e -> a where
  _ExpectedTyAll :: Prism' e (Type ki ty a)

instance AsExpectedTyAll (ErrExpectedTyAll ki ty a) ki ty a where
  _ExpectedTyAll = _ErrExpectedTyAll

instance {-# OVERLAPPABLE #-} AsExpectedTyAll (ErrSum xs) ki ty a => AsExpectedTyAll (ErrSum (x ': xs)) ki ty a where
  _ExpectedTyAll = _ErrNext . _ExpectedTyAll

instance {-# OVERLAPPING #-} AsExpectedTyAll (ErrSum (ErrExpectedTyAll ki ty a ': xs)) ki ty a where
  _ExpectedTyAll = _ErrNow . _ExpectedTyAll

expectTyAll :: (MonadError e m, AsExpectedTyAll e ki ty a, AsTySystemFw ki ty) => Type ki ty a -> m (Kind ki, Scope () (Type ki ty) a)
expectTyAll ty =
  case preview _TyAll ty of
    Just (k, s) -> return (k, s)
    _ -> throwing _ExpectedTyAll ty
