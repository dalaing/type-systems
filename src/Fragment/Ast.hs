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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.Ast (
    AstVar(..)
  , _ATyVar
  , _APtVar
  , _ATmVar
  , Ast(..)
  , AstConstraint
  , AstTransversable
  , AstBound
  , AstEq
  , AstOrd
  , AstShow
  , _AVar
  , _AType
  , _APattern
  , _ATerm
  , Type(..)
  , _TyVar
  , _TyTree
  , _Type
  , Pattern(..)
  , _PtVar
  , _PtTree
  , _Pattern
  , Term(..)
  , _TmVar
  ) where

import Control.Monad (ap)
import GHC.Exts (Constraint)
import Data.Traversable (fmapDefault, foldMapDefault)

import Control.Error

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Util

data AstVar a =
    ATyVar a
  | APtVar a
  | ATmVar a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''AstVar

deriveEq1 ''AstVar
deriveOrd1 ''AstVar
deriveShow1 ''AstVar

-- with type families, we could add annotations here by just adding some more constraints
data Ast ty pt tm a =
    AVar a
  | AType (ty (Ast ty pt tm) a)
  | APattern (pt (Ast ty pt tm) a)
  | ATerm (tm ty pt (Ast ty pt tm) a)

makePrisms ''Ast

type AstConstraint (k :: ((* -> *) -> * -> *) -> Constraint) ty pt tm = (k ty, k pt, k (tm ty pt))
type AstTransversable ty pt tm = AstConstraint (Bitransversable) ty pt tm
type AstBound ty pt tm = AstConstraint Bound ty pt tm
type AstEq ty pt tm = AstConstraint EqRec ty pt tm
type AstOrd ty pt tm = AstConstraint OrdRec ty pt tm
type AstShow ty pt tm = AstConstraint ShowRec ty pt tm

instance (Eq a, AstEq ty pt tm) => Eq (Ast ty pt tm a) where
  AVar x == AVar y = x == y
  AType x == AType y = eqRec x y
  APattern x == APattern y = eqRec x y
  ATerm x == ATerm y = eqRec x y
  _ == _ = False

instance AstEq ty pt tm => Eq1 (Ast ty pt tm) where
  liftEq e (AVar x) (AVar y) = e x y
  liftEq e (AType x) (AType y) = liftEq1Rec e x y
  liftEq e (APattern x) (APattern y) = liftEq1Rec e x y
  liftEq e (ATerm x) (ATerm y) = liftEq1Rec e x y
  liftEq _ _ _ = False

instance (Ord a, AstOrd ty pt tm) => Ord (Ast ty pt tm a) where
  compare (AVar x) (AVar y) = compare x y
  compare (AVar _) _ = LT
  compare _ (AVar _) = GT
  compare (AType x) (AType y) = compareRec x y
  compare (AType _) _ = LT
  compare _ (AType _) = GT
  compare (APattern x) (APattern y) = compareRec x y
  compare (APattern _) _ = LT
  compare _ (APattern _) = GT
  compare (ATerm x) (ATerm y) = compareRec x y

instance AstOrd ty pt tm => Ord1 (Ast ty pt tm) where
  liftCompare c (AVar x) (AVar y) = c x y
  liftCompare _ (AVar _) _ = LT
  liftCompare _ _ (AVar _) = GT
  liftCompare c (AType x) (AType y) = liftCompare1Rec c x y
  liftCompare _ (AType _) _ = LT
  liftCompare _ _ (AType _) = GT
  liftCompare c (APattern x) (APattern y) = liftCompare1Rec c x y
  liftCompare _ (APattern _) _ = LT
  liftCompare _ _ (APattern _) = GT
  liftCompare c (ATerm x) (ATerm y) = liftCompare1Rec c x y

instance (Show a, AstShow ty pt tm) => Show (Ast ty pt tm a) where
  showsPrec n (AVar x) = showsUnaryWith showsPrec "AVar" n x
  showsPrec n (AType x) = showsUnaryWith showsPrecRec "AType" n x
  showsPrec n (APattern x) = showsUnaryWith showsPrecRec "APattern" n x
  showsPrec n (ATerm x) = showsUnaryWith showsPrecRec "ATerm" n x

instance AstShow ty pt tm => Show1 (Ast ty pt tm) where
  liftShowsPrec s _ n (AVar x) = s n x
  liftShowsPrec s sl n (AType x) = liftShowsPrec1Rec s sl n x
  liftShowsPrec s sl n (APattern x) = liftShowsPrec1Rec s sl n x
  liftShowsPrec s sl n (ATerm x) = liftShowsPrec1Rec s sl n x

instance AstTransversable ty pt tm => Functor (Ast ty pt tm) where
  fmap = fmapDefault

instance AstTransversable ty pt tm => Foldable (Ast ty pt tm) where
  foldMap = foldMapDefault

instance AstTransversable ty pt tm => Traversable (Ast ty pt tm) where
  traverse f (AVar x) = AVar <$> f x
  traverse f (AType x) = AType <$> traverseDefault f x
  traverse f (APattern x) = APattern <$> traverseDefault f x
  traverse f (ATerm x) = ATerm <$> traverseDefault f x

instance (AstTransversable ty pt tm, AstBound ty pt tm) => Applicative (Ast ty pt tm) where
  pure = return
  (<*>) = ap

instance (AstTransversable ty pt tm, AstBound ty pt tm) => Monad (Ast ty pt tm) where
  return = AVar

  AVar x >>= f = f x
  AType ty >>= f = AType (ty >>>= f)
  APattern pt >>= f = APattern (pt >>>= f)
  ATerm tm >>= f = ATerm (tm >>>= f)

data Type ty a = TyVar a | TyTree (ty (Type ty) a)

makePrisms ''Type

typeToAst :: Bitransversable ty => Type ty a -> Ast ty pt tm (AstVar a)
typeToAst = runIdentity . typeToAst' (Identity . ATyVar)

typeToAst' :: Bitransversable ty => (a -> Identity b) -> Type ty a -> Identity (Ast ty pt tm b)
typeToAst' fV x = typeToAst'' =<< traverse fV x

typeToAst'' :: Bitransversable ty => Type ty a -> Identity (Ast ty pt tm a)
typeToAst'' (TyVar x) = Identity (AVar x)
typeToAst'' (TyTree ty) = fmap AType . bitransverse typeToAst' pure $ ty

astToType :: AstTransversable ty pt tm => Ast ty pt tm (AstVar a) -> Maybe (Type ty a)
astToType = astToType' fV
  where
    fV (ATyVar x) = Just x
    fV _ = Nothing

astToType' :: AstTransversable ty pt tm => (a -> Maybe b) -> Ast ty pt tm a -> Maybe (Type ty b)
astToType' fV x = astToType'' =<< traverse fV x

astToType'' :: (Bitransversable ty, Bitransversable pt, Bitransversable (tm ty pt)) => Ast ty pt tm a -> Maybe (Type ty a)
astToType'' (AVar x) = Just (TyVar x)
astToType'' (AType ty) = fmap TyTree . bitransverse astToType' pure $ ty
astToType'' _ = Nothing

_Type :: AstTransversable ty pt tm => Prism' (Ast ty pt tm (AstVar a)) (Type ty a)
_Type = prism typeToAst (\x -> note x . astToType $ x)

instance Bitransversable ty => Functor (Type ty) where
  fmap = fmapDefault

instance Bitransversable ty => Foldable (Type ty) where
  foldMap = foldMapDefault

instance Bitransversable ty => Traversable (Type ty) where
  traverse f (TyVar x) = TyVar <$> f x
  traverse f (TyTree x) = TyTree <$> traverseDefault f x

instance (Eq a, EqRec ty) => Eq (Type ty a) where
  TyVar x == TyVar y = (==) x y
  TyTree x == TyTree y = eqRec x y
  _ == _ = False

instance EqRec ty => Eq1 (Type ty) where
  liftEq e (TyVar x) (TyVar y) = e x y
  liftEq e (TyTree x) (TyTree y) = liftEq1Rec e x y
  liftEq _ _ _ = False

instance (Ord a, OrdRec ty) => Ord (Type ty a) where
  compare (TyVar x) (TyVar y) = compare x y
  compare (TyVar _) _ = LT
  compare _ (TyVar _) = GT
  compare (TyTree x) (TyTree y) = compareRec x y

instance OrdRec ty => Ord1 (Type ty) where
  liftCompare c (TyVar x) (TyVar y) = c x y
  liftCompare _ (TyVar _) _ = LT
  liftCompare _ _ (TyVar _) = GT
  liftCompare c (TyTree x) (TyTree y) = liftCompare1Rec c x y

instance (Show a, ShowRec ty) => Show (Type ty a) where
  showsPrec n (TyVar x) = showsUnaryWith showsPrec "TyVar" n x
  showsPrec n (TyTree x) = showsUnaryWith showsPrecRec "TyTree" n x

instance ShowRec ty => Show1 (Type ty) where
  liftShowsPrec s _ n (TyVar x) = s n x
  liftShowsPrec s sl n (TyTree x) = liftShowsPrec1Rec s sl n x

instance (Bound ty, Bitransversable ty) => Applicative (Type ty) where
  pure = return
  (<*>) = ap

instance (Bound ty, Bitransversable ty) => Monad (Type ty) where
  return = TyVar

  TyVar x >>= f = f x
  TyTree ty >>= f = TyTree (ty >>>= f)

data Pattern pt a = PtVar a | PtTree (pt (Pattern pt) a)

makePrisms ''Pattern

instance Bitransversable pt => Functor (Pattern pt) where
  fmap = fmapDefault

instance Bitransversable pt => Foldable (Pattern pt) where
  foldMap = foldMapDefault

instance Bitransversable pt => Traversable (Pattern pt) where
  traverse f (PtVar x) = PtVar <$> f x
  traverse f (PtTree x) = PtTree <$> traverseDefault f x

instance (Eq a, EqRec pt) => Eq (Pattern pt a) where
  PtVar x == PtVar y = (==) x y
  PtTree x == PtTree y = eqRec x y
  _ == _ = False

instance EqRec pt => Eq1 (Pattern pt) where
  liftEq e (PtVar x) (PtVar y) = e x y
  liftEq e (PtTree x) (PtTree y) = liftEq1Rec e x y
  liftEq _ _ _ = False

instance (Ord a, OrdRec pt) => Ord (Pattern pt a) where
  compare (PtVar x) (PtVar y) = compare x y
  compare (PtVar _) _ = LT
  compare _ (PtVar _) = GT
  compare (PtTree x) (PtTree y) = compareRec x y

instance OrdRec pt => Ord1 (Pattern pt) where
  liftCompare c (PtVar x) (PtVar y) = c x y
  liftCompare _ (PtVar _) _ = LT
  liftCompare _ _ (PtVar _) = GT
  liftCompare c (PtTree x) (PtTree y) = liftCompare1Rec c x y

instance (Show a, ShowRec pt) => Show (Pattern pt a) where
  showsPrec n (PtVar x) = showsUnaryWith showsPrec "PtVar" n x
  showsPrec n (PtTree x) = showsUnaryWith showsPrecRec "PtTree" n x

instance ShowRec pt => Show1 (Pattern pt) where
  liftShowsPrec s _ n (PtVar x) = s n x
  liftShowsPrec s sl n (PtTree x) = liftShowsPrec1Rec s sl n x

instance (Bound pt, Bitransversable pt) => Applicative (Pattern pt) where
  pure = return
  (<*>) = ap

instance (Bound pt, Bitransversable pt) => Monad (Pattern pt) where
  return = PtVar

  PtVar x >>= f = f x
  PtTree pt >>= f = PtTree (pt >>>= f)

patternToAst :: Bitransversable pt => Pattern pt a -> Ast ty pt tm (AstVar a)
patternToAst = runIdentity . patternToAst' (Identity . APtVar)

patternToAst' :: Bitransversable pt => (a -> Identity b) -> Pattern pt a -> Identity (Ast ty pt tm b)
patternToAst' fV x = patternToAst'' =<< traverse fV x

patternToAst'' :: Bitransversable pt => Pattern pt a -> Identity (Ast ty pt tm a)
patternToAst'' (PtVar x) = pure (AVar x)
patternToAst'' (PtTree pt) = fmap APattern . bitransverse patternToAst' pure $ pt

astToPattern :: AstTransversable ty pt tm => Ast ty pt tm (AstVar a) -> Maybe (Pattern pt a)
astToPattern = astToPattern' fV
  where
    fV (APtVar x) = Just x
    fV _ = Nothing

astToPattern' :: AstTransversable ty pt tm => (a -> Maybe b) -> Ast ty pt tm a -> Maybe (Pattern pt b)
astToPattern' fV x = astToPattern'' =<< traverse fV x

astToPattern'' :: AstTransversable ty pt tm => Ast ty pt tm a -> Maybe (Pattern pt a)
astToPattern'' (AVar x) = pure (PtVar x)
astToPattern'' (APattern pt) = fmap PtTree . bitransverse astToPattern' pure $ pt
astToPattern'' _ = Nothing

_Pattern :: AstTransversable ty pt tm => Prism' (Ast ty pt tm (AstVar a)) (Pattern pt a)
_Pattern = prism patternToAst (\x -> note x . astToPattern $ x)

-- Should we pack a TmVar / TmTree in here, and bury the Ast one level down?
-- The goal would be to have Scope () Term a instead of Scope () Ast (AstVar a) in the prism APIs
-- If we can hide Ast completely, we should
newtype Term ty pt tm a = Term (Ast ty pt tm (AstVar a))
  deriving (Eq, Ord, Show)

makeWrapped ''Term

instance AstEq ty pt tm => Eq1 (Term ty pt tm) where
  liftEq = $(makeLiftEq ''Term)

instance AstOrd ty pt tm => Ord1 (Term ty pt tm) where
  liftCompare = $(makeLiftCompare ''Term)

instance AstShow ty pt tm => Show1 (Term ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''Term)

deriving instance AstTransversable ty pt tm => Functor (Term ty pt tm)
deriving instance AstTransversable ty pt tm => Foldable (Term ty pt tm)
deriving instance AstTransversable ty pt tm => Traversable (Term ty pt tm)

_TmVar :: Prism' (Term ty pt tm a) a
_TmVar = _Wrapped . _AVar . _ATmVar
