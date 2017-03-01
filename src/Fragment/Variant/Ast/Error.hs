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
module Fragment.Variant.Ast.Error (
    ErrExpectedTyVariant(..)
  , AsExpectedTyVariant(..)
  , expectTyVariant
  , ErrVariantNotFound(..)
  , AsVariantNotFound(..)
  , lookupVariant
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens (preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import qualified Data.Text as T
import qualified Data.List.NonEmpty as N

import Ast.Type
import Ast.Error

import Fragment.Variant.Ast.Type

data ErrExpectedTyVariant ki ty a = ErrExpectedTyVariant (Type ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyVariant

class AsExpectedTyVariant e ki ty a where -- | e -> ty, e -> a where
  _ExpectedTyVariant :: Prism' e (Type ki ty a)

instance AsExpectedTyVariant (ErrExpectedTyVariant ki ty a) ki ty a where
  _ExpectedTyVariant = _ErrExpectedTyVariant

instance {-# OVERLAPPABLE #-} AsExpectedTyVariant ((ErrSum xs)) ki ty a => AsExpectedTyVariant (ErrSum (x ': xs)) ki ty a where
  _ExpectedTyVariant = _ErrNext . _ExpectedTyVariant

instance {-# OVERLAPPING #-} AsExpectedTyVariant (ErrSum (ErrExpectedTyVariant ki ty a ': xs)) ki ty a where
  _ExpectedTyVariant = _ErrNow . _ExpectedTyVariant

expectTyVariant :: (MonadError e m, AsExpectedTyVariant e ki ty a, AsTyVariant ki ty) => Type ki ty a -> m (N.NonEmpty (T.Text, Type ki ty a))
expectTyVariant ty =
  case preview _TyVariant ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyVariant ty

data ErrVariantNotFound = ErrVariantNotFound T.Text
  deriving (Eq, Ord, Show)

makePrisms ''ErrVariantNotFound

class AsVariantNotFound e where
  _VariantNotFound :: Prism' e T.Text

instance AsVariantNotFound ErrVariantNotFound where
  _VariantNotFound = _ErrVariantNotFound

instance {-# OVERLAPPABLE #-} AsVariantNotFound (ErrSum xs) => AsVariantNotFound (ErrSum (x ': xs)) where
  _VariantNotFound = _ErrNext . _VariantNotFound

instance {-# OVERLAPPING #-} AsVariantNotFound (ErrSum (ErrVariantNotFound ': xs)) where
  _VariantNotFound = _ErrNow . _VariantNotFound

lookupVariant :: (MonadError e m, AsVariantNotFound e) =>  N.NonEmpty (T.Text, t a) -> T.Text -> m (t a)
lookupVariant ts t =
  case lookup t (N.toList ts) of
    Just x -> return x
    Nothing -> throwing _VariantNotFound t
