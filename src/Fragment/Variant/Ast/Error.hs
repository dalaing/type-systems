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

data ErrExpectedTyVariant ty a = ErrExpectedTyVariant (Type ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyVariant

class AsExpectedTyVariant e ty a | e -> ty, e -> a where
  _ExpectedTyVariant :: Prism' e (Type ty a)

instance AsExpectedTyVariant (ErrExpectedTyVariant ty a) ty a where
  _ExpectedTyVariant = _ErrExpectedTyVariant

instance {-# OVERLAPPABLE #-} AsExpectedTyVariant ((ErrSum xs) ty pt tm a) ty a => AsExpectedTyVariant (ErrSum (x ': xs) ty pt tm a) ty a where
  _ExpectedTyVariant = _ErrNext . _ExpectedTyVariant

instance {-# OVERLAPPING #-} AsExpectedTyVariant (ErrSum (ErrExpectedTyVariant ty a ': xs) ty pt tm a) ty a where
  _ExpectedTyVariant = _ErrNow . _ExpectedTyVariant

expectTyVariant :: (MonadError e m, AsExpectedTyVariant e ty a, AsTyVariant ty) => Type ty a -> m (N.NonEmpty (T.Text, Type ty a))
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

instance {-# OVERLAPPABLE #-} AsVariantNotFound ((ErrSum xs) ty pt tm a) => AsVariantNotFound (ErrSum (x ': xs) ty pt tm a) where
  _VariantNotFound = _ErrNext . _VariantNotFound

instance {-# OVERLAPPING #-} AsVariantNotFound (ErrSum (ErrVariantNotFound ': xs) ty pt tm a) where
  _VariantNotFound = _ErrNow . _VariantNotFound

lookupVariant :: (MonadError e m, AsVariantNotFound e) =>  N.NonEmpty (T.Text, t a) -> T.Text -> m (t a)
lookupVariant ts t =
  case lookup t (N.toList ts) of
    Just x -> return x
    Nothing -> throwing _VariantNotFound t
