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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Typed.SystemF.Internal where

import Control.Monad (ap)

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

data TypeF f a =
    TyFArr (f a) (f a)
  | TyFAll (Scope () f a)
  | TyFInt
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TypeF

instance (Monad f, Eq1 f) => Eq1 (TypeF f) where
  liftEq = $(makeLiftEq ''TypeF)

instance (Monad f, Ord1 f) => Ord1 (TypeF f) where
  liftCompare = $(makeLiftCompare ''TypeF)

instance (Show1 f) => Show1 (TypeF f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TypeF)

instance Bound TypeF where
  TyFArr x y >>>= f = TyFArr (x >>= f) (y >>= f)
  TyFAll s >>>= f = TyFAll (s >>>= f)

  TyFInt >>>= _ = TyFInt

data TermF f a =
    TmFLam (f a) (Scope () f a)
  | TmFApp (f a) (f a)
  | TmFLamTy (Scope () f a)
  | TmFAppTy (f a) (f a)
  | TmFAdd (f a) (f a)
  | TmFInt Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TermF

instance (Monad f, Eq1 f) => Eq1 (TermF f) where
  liftEq = $(makeLiftEq ''TermF)

instance (Monad f, Ord1 f) => Ord1 (TermF f) where
  liftCompare = $(makeLiftCompare ''TermF)

instance (Show1 f) => Show1 (TermF f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TermF)

instance Bound TermF where
  TmFLam ty s >>>= f = TmFLam (ty >>= f) (s >>>= f)
  TmFApp x y >>>= f = TmFApp (x >>= f) (y >>= f)
  TmFLamTy s >>>= f = TmFLamTy (s >>>= f)
  TmFAppTy x y >>>= f = TmFAppTy (x >>= f) (y >>= f)

  TmFAdd x y >>>= f = TmFAdd (x >>= f) (y >>= f)
  TmFInt i >>>= _ = TmFInt i

data AST a =
    AVar a
  | AType (TypeF AST a)
  | ATerm (TermF AST a)
  deriving (Functor, Foldable, Traversable)

instance Applicative AST where
  pure = return
  (<*>) = ap

instance Monad AST where
  return = AVar

  AVar x >>= f = f x
  AType ty >>= f = AType (ty >>>= f)
  ATerm tm >>= f = ATerm (tm >>>= f)

makePrisms ''AST

class AsVar p a | p -> a where
  _Var :: Prism' p a

instance AsVar (AST a) a where
  _Var = _AVar

instance AsTypeF (AST a) AST a where
  _TypeF = _AType

instance AsTermF (AST a) AST a where
  _TermF = _ATerm

deriveEq1 ''AST
deriveOrd1 ''AST
deriveShow1 ''AST

instance (Eq a) => Eq (AST a) where (==) = eq1
instance (Ord a) => Ord (AST a) where compare = compare1
instance (Show a) => Show (AST a) where showsPrec = showsPrec1

data ASTVar a =
    ASTTyVar a
  | ASTTmVar a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''ASTVar
