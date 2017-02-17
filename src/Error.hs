{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Error (
    AsUnexpected(..)
  , expect
  , AsExpectedEq(..)
  , expectEq
  , AsExpectedAllEq(..)
  , expectAllEq
  , AsUnknownTypeError(..)
  ) where

import Control.Monad (unless)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import qualified Data.List.NonEmpty as N

import Control.Lens

class AsUnexpected e ty | e -> ty where
  _Unexpected :: Prism' e (ty, ty)

expect :: (Eq ty, MonadError e m, AsUnexpected e ty) => ty -> ty -> m ()
expect ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _Unexpected (ty1, ty2)

class AsExpectedEq e ty | e -> ty where
  _ExpectedEq :: Prism' e (ty, ty)

expectEq :: (Eq ty, MonadError e m, AsExpectedEq e ty) => ty -> ty -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _ExpectedEq (ty1, ty2)

class AsUnknownTypeError e where
  _UnknownTypeError :: Prism' e ()

class AsExpectedAllEq e ty | e -> ty where
  _ExpectedAllEq :: Prism' e (N.NonEmpty ty)

expectAllEq :: (Eq (ty a), MonadError e m, AsExpectedAllEq e (ty a)) => N.NonEmpty (ty a) -> m (ty a)
expectAllEq (ty N.:| tys)
  | all (== ty) tys = return ty
  | otherwise = throwing _ExpectedAllEq (ty N.:| tys)
