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
{-# LANGUAGE MultiParamTypeClasses #-}
module SystemF.Scratch (
  ) where

import Control.Monad (ap)
import Data.Void

import Control.Lens

import qualified Data.Text as T

import Bound
import Data.Functor.Classes
import Data.Deriving

data SVar a =
    VTerm a
  | VType a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data STerm b = STerm b
  deriving (Eq, Ord, Show)

data SType b = SType b
  deriving (Eq, Ord, Show)

abstractTm :: Monad f => (a -> Maybe b) -> f (SVar a) -> Scope (STerm b) f (SVar a)
abstractTm f exp =
  let
    g (VTerm x) = STerm <$> f x
    g _ = Nothing
  in
    abstract g exp

instantiateTm :: Monad f => (b -> f a) -> Scope (STerm b) f (SVar a) -> f (SVar a)
instantiateTm f s =
  let
    g (STerm b) = VTerm <$> f b
  in
    instantiate g s

abstractTy :: Monad f => (a -> Maybe b) -> f (SVar a) -> Scope (SType b) f (SVar a)
abstractTy f exp =
  let
    g (VType x) = SType <$> f x
    g _ = Nothing
  in
    abstract g exp

instantiateTy :: Monad f => (b -> f a) -> Scope (SType b) f (SVar a) -> f (SVar a)
instantiateTy f s =
  let
    g (SType b) = VType <$> f b
  in
    instantiate g s

data Type a =
    TyVar a
  | TyAll (Scope () Type a)
  | TyArr (Type a) (Type a)
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
  TyAll s >>= f = TyAll (s >>>= f)
  TyArr g x >>= f = TyArr (g >>= f) (x >>= f)

data Term a =
    TmVar a
  | TmLam (Type Void) (Scope () Term a)
  | TmApp (Term a) (Term a)
  | TmLamTy (Scope () Term a)
  | TmAppTy (Term a) (Type a)
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Term
deriveOrd1  ''Term
deriveShow1 ''Term

instance (Eq a) => Eq (Term a) where (==) = eq1
instance (Ord a) => Ord (Term a) where compare = compare1
instance (Show a) => Show (Term a) where showsPrec = showsPrec1

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = TmVar

  TmVar x >>= f = f x
  TmLam ty s >>= f = TmLam ty (s >>>= f)
  TmApp g x >>= f = TmApp (g >>= f) (x >>= f)
  TmLamTy tm >>= f = TmLamTy (tm >>>= f)
  TmAppTy tm ty >>= f = TmAppTy (tm >>= f) ty


data Blob a =
    BVar a
  | BTmLam (Blob a) (Scope () Blob a)
  | BTmApp (Blob a) (Blob a)
  | BTmLamTy (Scope () Blob a)
  | BTmAppTy (Blob a) (Blob a)
  | BTyAll (Scope () Blob a)
  | BTyArr (Blob a) (Blob a)
  | BTyInt
  | BTmInt Int
  | BTmAdd (Blob a) (Blob a)
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Blob
deriveOrd1  ''Blob
deriveShow1 ''Blob

instance (Eq a) => Eq (Blob a) where (==) = eq1
instance (Ord a) => Ord (Blob a) where compare = compare1
instance (Show a) => Show (Blob a) where showsPrec = showsPrec1

instance Applicative Blob where
  pure = return
  (<*>) = ap

instance Monad Blob where
  return = BVar

  BVar x >>= f = f x
  BTmLam t s >>= f = BTmLam (t >>= f) (s >>>= f)
  BTmApp g x >>= f = BTmApp (g >>= f) (x >>= f)
  BTmLamTy s >>= f = BTmLamTy (s >>>= f)
  BTmAppTy g x >>= f = BTmAppTy (g >>= f) (x >>= f)
  BTyAll s >>= f = BTyAll (s >>>= f)
  BTyArr g x >>= f = BTyArr (g >>= f) (x >>= f)
  BTyInt >>= _ = BTyInt
  BTmInt i >>= _ = BTmInt i
  BTmAdd x y >>= f = BTmAdd (x >>= f) (y >>= f)

newtype Term2 a = Term2 { unTerm :: Blob (SVar a) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

tmVar :: a -> Term2 a
tmVar = Term2 . BVar . VTerm

tmLam :: Eq a => a -> Type2 a -> Term2 a -> Term2 a
tmLam v (Type2 ty) = Term2 . BTmLam ty . abstract1 (VTerm v) . unTerm

tmApp :: Term2 a -> Term2 a -> Term2 a
tmApp (Term2 x) (Term2 y) = Term2 (BTmApp x y)

tmLamTy :: Eq a => a -> Term2 a -> Term2 a
tmLamTy v = Term2 . BTmLamTy . abstract1 (VType v) . unTerm

tmAppTy :: Term2 a -> Type2 a -> Term2 a
tmAppTy (Term2 x) (Type2 y) = Term2 (BTmAppTy x y)

tmInt :: Int -> Term2 a
tmInt i = Term2 (BTmInt i)

tmAdd :: Term2 a -> Term2 a -> Term2 a
tmAdd (Term2 x) (Term2 y) = Term2 (BTmAdd x y)

{-
eval :: Term2 a -> Term2 a
eval (Term2 (BTmInt x)) =
  Term2 (BTmInt x)
eval (Term2 (BTmAdd x y)) =
  case (eval (Term2 x), eval (Term2 y)) of
    (Term2 (BTmInt i), Term2 (BTmInt j)) -> Term2 (BTmInt (i + j))
    _ -> Term2 (BTmAdd x y)
-}

-- TODO the SVar thing probably won't work
-- - need to be able to do >>= for Term vars and also change the types of Type vars

-- TODO monad for Term2 and Type2
-- TODO eval
-- TODO type checking

newtype Type2 a = Type2 { unType :: Blob (SVar a) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

tyVar :: a -> Type2 a
tyVar = Type2 . BVar . VType

tyAll :: Eq a => a -> Type2 a -> Type2 a
tyAll v = Type2 . BTyAll . abstract1 (VType v) . unType

tyArr :: Type2 a -> Type2 a -> Type2 a
tyArr (Type2 x) (Type2 y) = Type2 (BTyArr x y)

tyInt :: Type2 a
tyInt = Type2 BTyInt

