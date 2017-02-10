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
module SystemF.Type (
  ) where

import Control.Monad (ap)
import Data.Void

import Bound
import Data.Functor.Classes
import Data.Deriving

data Type a =
    TyVar a
  | TyArr (Type a) (Type a)
  | TyAll (Scope () Type a)
  | TyInt
  | TyBool
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
  TyAll s >>= f = TyAll (s >>>= f)

-- use the same variable for both terms and types?
data Term ty a =
    TmVar a
  | TmAppTm (Term ty a) (Term ty a)
  | TmLamTm (ty a) (Scope () (Term ty) a)

  | TmAppTy (Term ty a) (ty a)
  | TmLamTy (Term (Scope () ty) a)

  | TmInt Int
  | TmBool Bool
  | TmAdd (Term ty a) (Term ty a)
  | TmAnd (Term ty a) (Term ty a)
  | TmEq (Term ty a) (Term ty a)
  | TmLt (Term ty a) (Term ty a)
  | TmIf (Term ty a) (Term ty a) (Term ty a)
  | TmAnn (ty a) (Term ty a)
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Term
deriveOrd1  ''Term
deriveShow1 ''Term

instance (Eq a) => Eq (Term ty a) where (==) = eq1
instance (Ord a) => Ord (Term ty a) where compare = compare1
instance (Show a) => Show (Term ty a) where showsPrec = showsPrec1

instance Applicative (Term ty) where
  pure = return
  (<*>) = ap

instance Monad (Term ty) where
  return = TmVar

  TmVar x >>= f = f x
  TmLamTm ty s >>= f = TmLamTm ty (s >>>= f)
  TmAppTm g x >>= f = TmAppTm (g >>= f) (x >>= f)

  -- TmLamTy tm >>= f = TmLamTy (tm >>>= f)
  -- TmAppTy tm ty >>= f = TmAppTy (tm >>= f) ty

  TmInt i >>= _ = TmInt i
  TmBool b >>= _ = TmBool b
  TmAdd x y >>= f = TmAdd (x >>= f) (y >>= f)
  TmAnd x y >>= f = TmAnd (x >>= f) (y >>= f)
  TmEq x y >>= f = TmEq (x >>= f) (y >>= f)
  TmLt x y >>= f = TmLt (x >>= f) (y >>= f)
  TmIf b t e >>= f = TmIf (b >>= f) (t >>= f) (e >>= f)
  TmAnn ty tm >>= f = TmAnn ty (tm >>= f)
