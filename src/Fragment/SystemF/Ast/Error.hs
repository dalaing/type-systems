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
module Fragment.SystemF.Ast.Error (
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

import Ast.Type
import Ast.Error

import Fragment.SystemF.Ast.Type

data ErrExpectedTyArr (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrExpectedTyArr (Type ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyArr

class AsExpectedTyArr e ty a | e -> ty, e -> a where
  _ExpectedTyArr :: Prism' e (Type ty a)

instance AsExpectedTyArr (ErrExpectedTyArr ty pt tm a) ty a where
  _ExpectedTyArr = _ErrExpectedTyArr

instance {-# OVERLAPPABLE #-} AsExpectedTyArr ((ErrSum xs) ty pt tm a) ty a => AsExpectedTyArr (ErrSum (x ': xs) ty pt tm a) ty a where
  _ExpectedTyArr = _ErrNext . _ExpectedTyArr

instance {-# OVERLAPPING #-} AsExpectedTyArr (ErrSum (ErrExpectedTyArr ': xs) ty pt tm a) ty a where
  _ExpectedTyArr = _ErrNow . _ExpectedTyArr

expectTyArr :: (MonadError e m, AsExpectedTyArr e ty a, AsTySystemF ty) => Type ty a -> m (Type ty a, Type ty a)
expectTyArr ty =
  case preview _TyArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedTyArr ty

data ErrExpectedTyAll (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrExpectedTyAll (Type ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyAll

class AsExpectedTyAll e ty a | e -> ty, e -> a where
  _ExpectedTyAll :: Prism' e (Type ty a)

instance AsExpectedTyAll (ErrExpectedTyAll ty pt tm a) ty a where
  _ExpectedTyAll = _ErrExpectedTyAll

instance {-# OVERLAPPABLE #-} AsExpectedTyAll ((ErrSum xs) ty pt tm a) ty a => AsExpectedTyAll (ErrSum (x ': xs) ty pt tm a) ty a where
  _ExpectedTyAll = _ErrNext . _ExpectedTyAll

instance {-# OVERLAPPING #-} AsExpectedTyAll (ErrSum (ErrExpectedTyAll ': xs) ty pt tm a) ty a where
  _ExpectedTyAll = _ErrNow . _ExpectedTyAll

expectTyAll :: (MonadError e m, AsExpectedTyAll e ty a, AsTySystemF ty) => Type ty a -> m (Scope () (Type ty) a)
expectTyAll ty =
  case preview _TyAll ty of
    Just s -> return s
    _ -> throwing _ExpectedTyAll ty
