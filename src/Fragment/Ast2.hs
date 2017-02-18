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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Fragment.Ast2 (
  ) where

import Control.Monad (ap)
import Data.Traversable

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Util

-- Types

data TypeF ty (k :: * -> *) a = TyVar a | TyTree (ty k a)

makePrisms ''TypeF

instance (Eq a, Eq1 k, EqRec ty) => Eq (TypeF ty k a) where
  TyVar x == TyVar y = (==) x y
  TyTree x == TyTree y = eqRec x y
  _ == _ = False

instance (Eq1 k, EqRec ty) => Eq1 (TypeF ty k) where
  liftEq e (TyVar x) (TyVar y) = e x y
  liftEq e (TyTree x) (TyTree y) = liftEqRec (liftEq e) e x y
  liftEq _ _ _ = False

instance (Ord a, Ord1 k, OrdRec ty) => Ord (TypeF ty k a) where
  compare (TyVar x) (TyVar y) = compare x y
  compare (TyTree x) (TyTree y) = compareRec x y
  compare (TyVar _) (TyTree _) = LT
  compare (TyTree _) (TyVar _) = GT

instance (Ord1 k, OrdRec ty) => Ord1 (TypeF ty k) where
  liftCompare c (TyVar x) (TyVar y) = c x y
  liftCompare c (TyTree x) (TyTree y) = liftCompareRec (liftCompare c) c x y
  liftCompare _ (TyVar _) (TyTree _) = LT
  liftCompare _ (TyTree _) (TyVar _) = GT

instance (Show a, Show1 k, ShowRec ty) => Show (TypeF ty k a) where
  showsPrec n (TyVar x) = showsUnaryWith showsPrec "TyVar" n x
  showsPrec n (TyTree x) = showsUnaryWith showsPrecRec "TyTree" n x

instance (Show1 k, ShowRec ty) => Show1 (TypeF ty k) where
  liftShowsPrec s _ n (TyVar x) = s n x
  liftShowsPrec s sl n (TyTree x) = liftShowsPrecRec (liftShowsPrec s sl) (liftShowList s sl) s sl n x

instance (Bitransversable ty, Traversable k) => Functor (TypeF ty k) where
  fmap = fmapDefault

instance (Bitransversable ty, Traversable k) => Foldable (TypeF ty k) where
  foldMap = foldMapDefault

instance (Bitransversable ty, Traversable k) => Traversable (TypeF ty k) where
  traverse = traverseDefault

instance Bitransversable ty => Bitransversable (TypeF ty) where
  bitransverse _ fL (TyVar x) = TyVar <$> fL x
  bitransverse fT fL (TyTree x) = TyTree <$> bitransverse fT fL x

newtype Type ty a = Type (TypeF ty (Type ty) a)
  deriving (Functor, Foldable, Traversable)

makeWrapped ''Type

instance (EqRec ty) => Eq1 (Type ty) where
  liftEq = $(makeLiftEq ''Type)

instance (OrdRec ty) => Ord1 (Type ty) where
  liftCompare = $(makeLiftCompare ''Type)

instance (ShowRec ty) => Show1 (Type ty) where
  liftShowsPrec = $(makeLiftShowsPrec ''Type)

deriving instance (Eq a, EqRec ty) => Eq (Type ty a)
deriving instance (Ord a, OrdRec ty) => Ord (Type ty a)
deriving instance (Show a, ShowRec ty) => Show (Type ty a)

instance (Bound ty, Bitransversable ty) => Applicative (Type ty) where
  pure = return
  (<*>) = ap

instance (Bound ty, Bitransversable ty) => Monad (Type ty) where
  return = Type . TyVar

  Type (TyVar x) >>= f = f x
  Type (TyTree ty) >>= f = Type (TyTree (ty >>>= f))

-- Patterns

data PatternF pt (k :: * -> *) a = PtVar a | PtTree (pt k a)

makePrisms ''PatternF

instance (Eq a, Eq1 k, EqRec pt) => Eq (PatternF pt k a) where
  PtVar x == PtVar y = (==) x y
  PtTree x == PtTree y = eqRec x y
  _ == _ = False

instance (Eq1 k, EqRec pt) => Eq1 (PatternF pt k) where
  liftEq e (PtVar x) (PtVar y) = e x y
  liftEq e (PtTree x) (PtTree y) = liftEqRec (liftEq e) e x y
  liftEq _ _ _ = False

instance (Ord a, Ord1 k, OrdRec pt) => Ord (PatternF pt k a) where
  compare (PtVar x) (PtVar y) = compare x y
  compare (PtTree x) (PtTree y) = compareRec x y
  compare (PtVar _) (PtTree _) = LT
  compare (PtTree _) (PtVar _) = GT

instance (Ord1 k, OrdRec pt) => Ord1 (PatternF pt k) where
  liftCompare c (PtVar x) (PtVar y) = c x y
  liftCompare c (PtTree x) (PtTree y) = liftCompareRec (liftCompare c) c x y
  liftCompare _ (PtVar _) (PtTree _) = LT
  liftCompare _ (PtTree _) (PtVar _) = GT

instance (Show a, Show1 k, ShowRec pt) => Show (PatternF pt k a) where
  showsPrec n (PtVar x) = showsUnaryWith showsPrec "PtVar" n x
  showsPrec n (PtTree x) = showsUnaryWith showsPrecRec "PtTree" n x

instance (Show1 k, ShowRec pt) => Show1 (PatternF pt k) where
  liftShowsPrec s _ n (PtVar x) = s n x
  liftShowsPrec s sl n (PtTree x) = liftShowsPrecRec (liftShowsPrec s sl) (liftShowList s sl) s sl n x

instance (Bitransversable pt, Traversable k) => Functor (PatternF pt k) where
  fmap = fmapDefault

instance (Bitransversable pt, Traversable k) => Foldable (PatternF pt k) where
  foldMap = foldMapDefault

instance (Bitransversable pt, Traversable k) => Traversable (PatternF pt k) where
  traverse = traverseDefault

instance Bitransversable pt => Bitransversable (PatternF pt) where
  bitransverse _ fL (PtVar x) = PtVar <$> fL x
  bitransverse fT fL (PtTree x) = PtTree <$> bitransverse fT fL x

newtype Pattern pt a = Pattern (PatternF pt (Pattern pt) a)
  deriving (Functor, Foldable, Traversable)

makeWrapped ''Pattern

instance (EqRec pt) => Eq1 (Pattern pt) where
  liftEq = $(makeLiftEq ''Pattern)

instance (OrdRec pt) => Ord1 (Pattern pt) where
  liftCompare = $(makeLiftCompare ''Pattern)

instance (ShowRec pt) => Show1 (Pattern pt) where
  liftShowsPrec = $(makeLiftShowsPrec ''Pattern)

deriving instance (Eq a, EqRec pt) => Eq (Pattern pt a)
deriving instance (Ord a, OrdRec pt) => Ord (Pattern pt a)
deriving instance (Show a, ShowRec pt) => Show (Pattern pt a)

instance (Bound pt, Bitransversable pt) => Applicative (Pattern pt) where
  pure = return
  (<*>) = ap

instance (Bound pt, Bitransversable pt) => Monad (Pattern pt) where
  return = Pattern . PtVar

  Pattern (PtVar x) >>= f = f x
  Pattern (PtTree pt) >>= f = Pattern (PtTree (pt >>>= f))

-- Terms

data TermF (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) tm (k :: * -> *) a = TmVar a | TmTree (tm ty pt k a)

makePrisms ''TermF

instance (Eq a, Eq1 k, EqRec (tm ty pt)) => Eq (TermF ty pt tm k a) where
  TmVar x == TmVar y = (==) x y
  TmTree x == TmTree y = eqRec x y
  _ == _ = False

instance (Eq1 k, EqRec (tm ty pt)) => Eq1 (TermF ty pt tm k) where
  liftEq e (TmVar x) (TmVar y) = e x y
  liftEq e (TmTree x) (TmTree y) = liftEqRec (liftEq e) e x y
  liftEq _ _ _ = False

instance (Ord a, Ord1 k, OrdRec (tm ty pt)) => Ord (TermF ty pt tm k a) where
  compare (TmVar x) (TmVar y) = compare x y
  compare (TmTree x) (TmTree y) = compareRec x y
  compare (TmVar _) (TmTree _) = LT
  compare (TmTree _) (TmVar _) = GT

instance (Ord1 k, OrdRec (tm ty pt)) => Ord1 (TermF ty pt tm k) where
  liftCompare c (TmVar x) (TmVar y) = c x y
  liftCompare c (TmTree x) (TmTree y) = liftCompareRec (liftCompare c) c x y
  liftCompare _ (TmVar _) (TmTree _) = LT
  liftCompare _ (TmTree _) (TmVar _) = GT

instance (Show a, Show1 k, ShowRec (tm ty pt)) => Show (TermF ty pt tm k a) where
  showsPrec n (TmVar x) = showsUnaryWith showsPrec "TmVar" n x
  showsPrec n (TmTree x) = showsUnaryWith showsPrecRec "TmTree" n x

instance (Show1 k, ShowRec (tm ty pt)) => Show1 (TermF ty pt tm k) where
  liftShowsPrec s _ n (TmVar x) = s n x
  liftShowsPrec s sl n (TmTree x) = liftShowsPrecRec (liftShowsPrec s sl) (liftShowList s sl) s sl n x

instance (Bitransversable (tm ty pt), Traversable k) => Functor (TermF ty pt tm k) where
  fmap = fmapDefault

instance (Bitransversable (tm ty pt), Traversable k) => Foldable (TermF ty pt tm k) where
  foldMap = foldMapDefault

instance (Bitransversable (tm ty pt), Traversable k) => Traversable (TermF ty pt tm k) where
  traverse = traverseDefault

instance Bitransversable (tm ty pt) => Bitransversable (TermF ty pt tm) where
  bitransverse _ fL (TmVar x) = TmVar <$> fL x
  bitransverse fT fL (TmTree x) = TmTree <$> bitransverse fT fL x

data AstF ty pt tm k a =
    AType (TypeF ty k a)
  | APattern (PatternF pt k a)
  | ATerm (TermF ty pt tm k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''AstF

instance (Eq1 k, EqRec ty, EqRec pt, EqRec (tm ty pt)) => Eq1 (AstF ty pt tm k) where
  liftEq e (AType x) (AType y) = liftEq e x y
  liftEq e (APattern x) (APattern y) = liftEq e x y
  liftEq e (ATerm x) (ATerm y) = liftEq e x y
  liftEq _ _ _ = False

instance (Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Bitransversable (AstF ty pt tm) where
  bitransverse fT fL (AType ty) = AType <$> bitransverse fT fL ty
  bitransverse fT fL (APattern pt) = APattern <$> bitransverse fT fL pt
  bitransverse fT fL (ATerm tm) = ATerm <$> bitransverse fT fL tm

newtype Ast ty pt tm a = Ast (AstF ty pt tm (Ast ty pt tm) a)
  deriving (Functor, Foldable, Traversable)

makeWrapped ''Ast

instance (EqRec ty, EqRec pt, EqRec (tm ty pt)) => Eq1 (Ast ty pt tm) where
  liftEq = $(makeLiftEq ''Ast)

deriving instance (Eq a, EqRec ty, EqRec pt, EqRec (tm ty pt)) => Eq (Ast ty pt tm a)

-- TODO these is symmetry here, which we should be able to exploit to get to generic prism
-- we want Iso' a p and Prism' b p and Bitransversable p to get Prism' a b
-- possibly also need Bitransversable for a and b

-- can we do this with an Ast that doesn't use k
-- ie that uses AType (Type ty a) | APattern (Pattern pt a) | etc ...
-- yes, and it becomes more or less trivial

-- if TypeF didn't have the var in it, but Type did
-- and AstF didn't have vars in it, but Ast did and used AstVar internally
-- - there are probably isos and prisms betwen Ast and Type, etc..
-- - there are probably Bound instances for AstF and TypeF etc...

patternToAst :: Bitransversable pt => Pattern pt a -> Identity (Ast ty pt tm a)
patternToAst (Pattern pt) = fmap (Ast . APattern) . bitransverse (\f p -> (>>= patternToAst) $ traverse f p) pure $ pt

astToPattern :: (Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Ast ty pt tm a -> Maybe (Pattern pt a)
astToPattern (Ast (APattern pt)) = fmap Pattern . bitransverse (\f p -> (>>= astToPattern) $ traverse f p) pure $ pt

-- This is only a win if we can build a Monad instance for the new Term

{-
instance (OrdRec ty, OrdRec pt, OrdRec (tm ty pt)) => Ord1 (Ast ty pt tm) where
  liftCompare = $(makeLiftCompare ''Pattern)

deriving instance (Ord a, OrdRec ty, OrdRec pt, OrdRec (tm ty pt)) => Ord (Ast ty pt tm a)

instance (ShowRec ty, ShowRec pt, ShowRec (tm ty pt)) => Show1 (Ast ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''Pattern)

deriving instance (Show a, ShowRec ty, ShowRec pt, ShowRec (tm ty pt)) => Show (Ast ty pt tm a)
-}


newtype Term ty pt tm a = Term (TermF ty pt tm (Ast ty pt tm) a)
  deriving (Functor, Foldable, Traversable)

makeWrapped ''Term

{-
instance Bound (tm ty pt) => Monad (Term ty pt tm) where
  return = Term . TmVar
  Term (TmVar x) >>= f = f x


data TmAst ty pt tm a =
    AType (Type ty a)
  | APattern (Pattern pt a)
  | ATerm (Term ty pt tm a)

instance (Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Functor (TmAst ty pt tm) where
  fmap = fmapDefault

instance (Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Foldable (TmAst ty pt tm) where
  foldMap = foldMapDefault

instance (Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Traversable (TmAst ty pt tm) where
  traverse f (AType ty) = AType <$> traverse f ty
  traverse f (APattern pt) = APattern <$> traverse f pt
  traverse f (ATerm tm) = ATerm <$> traverse f tm

data Term ty pt tm a = TmVar a | TmTree (tm ty pt (TmAst ty pt tm) a)

instance (Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Functor (Term ty pt tm) where
  fmap = fmapDefault

instance (Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Foldable (Term ty pt tm) where
  foldMap = foldMapDefault

instance (Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Traversable (Term ty pt tm) where
  traverse f (TmVar x) = TmVar <$> f x
  traverse f (TmTree x) = TmTree <$> bitransverse traverse f x

makePrisms ''TmAst
makePrisms ''Term

instance (Bound (tm ty pt), Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Applicative (Term ty pt tm) where
  pure = return
  (<*>) = ap

instance (Bound (tm ty pt), Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Monad (Term ty pt tm) where
  return = TmVar

  TmVar x >>= f = f x
  -- TmTree tm >>= f = TmTree (tm >>>= f)
-}

