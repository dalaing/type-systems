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
module STLC.Term (
    Term(..)
  , lam
  ) where

import Control.Monad (ap)
import Data.Void

import qualified Data.Text as T

import Bound
import Data.Functor.Classes
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import STLC.Type

data Term a =
    TmVar a
  | TmLam T.Text (Type Void) (Scope () Term a)
  | TmApp (Term a) (Term a)
  | TmInt Int
  | TmBool Bool
  | TmAdd (Term a) (Term a)
  | TmAnd (Term a) (Term a)
  | TmEq (Term a) (Term a)
  | TmLt (Term a) (Term a)
  | TmIf (Term a) (Term a) (Term a)
  | TmAnn (Type Void) (Term a)
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Term
deriveOrd1  ''Term
deriveShow1 ''Term

instance Eq a => Eq (Term a) where (==) = eq1
instance Ord a => Ord (Term a) where compare = compare1
instance Show a => Show (Term a) where showsPrec = showsPrec1

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = TmVar

  TmVar x >>= f = f x
  TmLam v ty s >>= f = TmLam v ty (s >>>= f)
  TmApp g x >>= f = TmApp (g >>= f) (x >>= f)
  TmInt i >>= _ = TmInt i
  TmBool b >>= _ = TmBool b
  TmAdd x y >>= f = TmAdd (x >>= f) (y >>= f)
  TmAnd x y >>= f = TmAnd (x >>= f) (y >>= f)
  TmEq x y >>= f = TmEq (x >>= f) (y >>= f)
  TmLt x y >>= f = TmLt (x >>= f) (y >>= f)
  TmIf b t e >>= f = TmIf (b >>= f) (t >>= f) (e >>= f)
  TmAnn ty tm >>= f = TmAnn ty (tm >>= f)

lam :: T.Text -> Type Void -> Term T.Text -> Term T.Text
lam v ty e = TmLam v ty (abstract1 v e)
