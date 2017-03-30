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
module Fragment.TyAll.Ast.Error (
    ErrExpectedTyAll(..)
  , AsExpectedTyAll(..)
  , expectTyAll
  , ErrExpectedTyAllAnnotation(..)
  , AsExpectedTyAllAnnotation(..)
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

import Fragment.TyAll.Ast.Type

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

expectTyAll :: (MonadError e m, AsExpectedTyAll e ki ty a, AsTyAll ki ty) => Type ki ty a -> m (Maybe (Kind ki a), Scope () (TyAst ki ty) (TyAstVar a))
expectTyAll ty =
  case preview _TyAll ty of
    Just (k, s) -> return (k, s)
    _ -> throwing _ExpectedTyAll ty

data ErrExpectedTyAllAnnotation = ErrExpectedTyAllAnnotation
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyAllAnnotation

class AsExpectedTyAllAnnotation e where -- | e -> ty, e -> a where
  _ExpectedTyAllAnnotation :: Prism' e ()

instance AsExpectedTyAllAnnotation ErrExpectedTyAllAnnotation where
  _ExpectedTyAllAnnotation = _ErrExpectedTyAllAnnotation

instance {-# OVERLAPPABLE #-} AsExpectedTyAllAnnotation (ErrSum xs) => AsExpectedTyAllAnnotation (ErrSum (x ': xs)) where
  _ExpectedTyAllAnnotation = _ErrNext . _ExpectedTyAllAnnotation

instance {-# OVERLAPPING #-} AsExpectedTyAllAnnotation (ErrSum (ErrExpectedTyAllAnnotation ': xs)) where
  _ExpectedTyAllAnnotation = _ErrNow . _ExpectedTyAllAnnotation
