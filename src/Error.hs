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
module Error (
    Expected(..)
  , Actual(..)
  , AsUnexpected(..)
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

import Fragment.Ast

import Util

newtype Expected ty a = Expected (Type ty a)
  deriving (Eq, Ord, Show)

newtype Actual ty a = Actual (Type ty a)
  deriving (Eq, Ord, Show)

class AsUnexpected e ty a | e -> ty, e -> a where
  _Unexpected :: Prism' e (Expected ty a, Actual ty a)

expect :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a) => Expected ty a -> Actual ty a -> m ()
expect e@(Expected ty1) a@(Actual ty2) =
  unless (ty1 == ty2) $
    throwing _Unexpected (e, a)

class AsExpectedEq e ty a | e -> ty, e -> a where
  _ExpectedEq :: Prism' e (Type ty a, Type ty a)

expectEq :: (Eq a, EqRec ty, MonadError e m, AsExpectedEq e ty a) => Type ty a -> Type ty a -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _ExpectedEq (ty1, ty2)

class AsUnknownTypeError e where
  _UnknownTypeError :: Prism' e ()

class AsExpectedAllEq e ty a | e -> ty, e -> a where
  _ExpectedAllEq :: Prism' e (N.NonEmpty (Type ty a))

expectAllEq :: (Eq a, EqRec ty, MonadError e m, AsExpectedAllEq e ty a) => N.NonEmpty (Type ty a) -> m (Type ty a)
expectAllEq (ty N.:| tys)
  | all (== ty) tys = return ty
  | otherwise = throwing _ExpectedAllEq (ty N.:| tys)
