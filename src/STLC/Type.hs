{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module STLC.Type (
    Type(..)
  , TypeSchemeF(..)
  , TypeScheme
  ) where

import Control.Monad (ap)

import Bound
import Data.Functor.Classes
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

data Type a =
    TyVar a
  | TyInt
  | TyBool
  | TyArr (Type a) (Type a)
  | TyForAll Int (Scope Int Type a)
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Type
deriveOrd1  ''Type
deriveShow1 ''Type

instance Eq a => Eq (Type a) where (==) = eq1
instance Ord a => Ord (Type a) where compare = compare1
instance Show a => Show (Type a) where showsPrec = showsPrec1

instance Applicative Type where
  pure = return
  (<*>) = ap

instance Monad Type where
  return = TyVar

  TyVar x >>= f = f x
  TyInt >>= _ = TyInt
  TyBool >>= _ = TyBool
  TyArr x y >>= f = TyArr (x >>= f) (y >>= f)
  TyForAll i s >>= f = TyForAll i (s >>>= f)

data TypeSchemeF ty a =
  TypeScheme Int (Scope Int ty a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bound TypeSchemeF where
  TypeScheme i s >>>= f = TypeScheme i (s >>>= f)

type TypeScheme = TypeSchemeF Type

