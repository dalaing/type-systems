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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Fragment.Ast (
    AstVar(..)
  , _ATyVar
  , _APtVar
  , _ATmVar
  , Ast(..)
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
  , TripleConstraint
  , TripleConstraint1
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

deriveEq1 ''AstVar
deriveOrd1 ''AstVar
deriveShow1 ''AstVar

makePrisms ''AstVar

-- with type families, we could add annotations here by just adding some more constraints
data Ast ty pt tm a =
    AVar a
  | AType (ty (Ast ty pt tm) a)
  | APattern (pt (Ast ty pt tm) a)
  | ATerm (tm ty pt (Ast ty pt tm) a)

makePrisms ''Ast

type TripleConstraint (k :: * -> Constraint) ty pt tm a =
  ( k (ty (Ast ty pt tm) a)
  , k (pt (Ast ty pt tm) a)
  , k (tm ty pt (Ast ty pt tm) a)
  )

type TripleConstraint1 (k :: (* -> *) -> Constraint) ty pt tm =
  ( k (ty (Ast ty pt tm))
  , k (pt (Ast ty pt tm))
  , k (tm ty pt (Ast ty pt tm))
  )

deriving instance (Eq a, TripleConstraint Eq ty pt tm a) => Eq (Ast ty pt tm a)

deriving instance (Ord a, TripleConstraint Ord ty pt tm a) => Ord (Ast ty pt tm a)

deriving instance (Show a, TripleConstraint Show ty pt tm a) => Show (Ast ty pt tm a)

instance TripleConstraint1 Eq1 ty pt tm => Eq1 (Ast ty pt tm) where
  liftEq = $(makeLiftEq ''Ast)

instance TripleConstraint1 Ord1 ty pt tm => Ord1 (Ast ty pt tm) where
  liftCompare = $(makeLiftCompare ''Ast)

instance TripleConstraint1 Show1 ty pt tm => Show1 (Ast ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''Ast)

deriving instance TripleConstraint1 Functor ty pt tm => Functor (Ast ty pt tm)
deriving instance TripleConstraint1 Foldable ty pt tm => Foldable (Ast ty pt tm)

deriving instance TripleConstraint1 Traversable ty pt tm => Traversable (Ast ty pt tm)

instance (TripleConstraint1 Functor ty pt tm, Bound ty, Bound pt, Bound (tm ty pt)) => Applicative (Ast ty pt tm) where
  pure = return
  (<*>) = ap

instance (TripleConstraint1 Functor ty pt tm, Bound ty, Bound pt, Bound (tm ty pt)) => Monad (Ast ty pt tm) where
  return = AVar

  AVar x >>= f = f x
  AType ty >>= f = AType (ty >>>= f)
  APattern pt >>= f = APattern (pt >>>= f)
  ATerm tm >>= f = ATerm (tm >>>= f)

data Type ty a = TyVar a | TyTree (ty (Type ty) a)

makePrisms ''Type

typeToAst :: (Traversable (ty (Type ty)), Bitransversable ty) => Type ty a -> Ast ty pt tm (AstVar a)
typeToAst = runIdentity . typeToAst' (Identity . ATyVar)

typeToAst' :: (Traversable (ty (Type ty)), Bitransversable ty) => (a -> Identity b) -> Type ty a -> Identity (Ast ty pt tm b)
typeToAst' fV x = typeToAst'' =<< traverse fV x

typeToAst'' :: (Traversable (ty (Type ty)), Bitransversable ty) => Type ty a -> Identity (Ast ty pt tm a)
typeToAst'' (TyVar x) = Identity (AVar x)
typeToAst'' (TyTree ty) = fmap AType . bitransverse typeToAst' pure $ ty

astToType :: (TripleConstraint1 Traversable ty pt tm, Bitransversable ty) => Ast ty pt tm (AstVar a) -> Maybe (Type ty a)
astToType = astToType' fV
  where
    fV (ATyVar x) = Just x
    fV _ = Nothing

astToType' :: (TripleConstraint1 Traversable ty pt tm, Bitransversable ty) => (a -> Maybe b) -> Ast ty pt tm a -> Maybe (Type ty b)
astToType' fV x = astToType'' =<< traverse fV x

astToType'' :: (TripleConstraint1 Traversable ty pt tm, Bitransversable ty) => Ast ty pt tm a -> Maybe (Type ty a)
astToType'' (AVar x) = Just (TyVar x)
astToType'' (AType ty) = fmap TyTree . bitransverse astToType' pure $ ty
astToType'' _ = Nothing

_Type :: (Traversable (ty (Type ty)), TripleConstraint1 Traversable ty pt tm, Bitransversable ty) => Prism' (Ast ty pt tm (AstVar a)) (Type ty a)
_Type = prism typeToAst (\x -> note x . astToType $ x)

instance Bitransversable ty => Functor (Type ty) where
  fmap = fmapDefault

instance Bitransversable ty => Foldable (Type ty) where
  foldMap = foldMapDefault

instance Bitransversable ty => Traversable (Type ty) where
  traverse f (TyVar x) = TyVar <$> f x
  traverse f (TyTree x) = TyTree <$> bitransverse traverse f x

instance (Eq a, EqRec ty) => Eq (Type ty a) where
  TyVar x == TyVar y = (==) x y
  TyTree x == TyTree y = eqRec x y
  _ == _ = False

instance EqRec ty => Eq1 (Type ty) where
  liftEq e (TyVar x) (TyVar y) = e x y
  liftEq e (TyTree x) (TyTree y) = liftEqRec (liftEq e) e x y
  liftEq _ _ _ = False

instance (Ord a, OrdRec ty) => Ord (Type ty a) where
  compare (TyVar x) (TyVar y) = compare x y
  compare (TyTree x) (TyTree y) = compareRec x y
  compare (TyVar _) (TyTree _) = LT
  compare (TyTree _) (TyVar _) = GT

instance OrdRec ty => Ord1 (Type ty) where
  liftCompare c (TyVar x) (TyVar y) = c x y
  liftCompare c (TyTree x) (TyTree y) = liftCompareRec (liftCompare c) c x y
  liftCompare _ (TyVar _) (TyTree _) = LT
  liftCompare _ (TyTree _) (TyVar _) = GT

instance (Show a, ShowRec ty) => Show (Type ty a) where
  showsPrec n (TyVar x) = showsUnaryWith showsPrec "TyVar" n x
  showsPrec n (TyTree x) = showsUnaryWith showsPrecRec "TyTree" n x

instance ShowRec ty => Show1 (Type ty) where
  liftShowsPrec s _ n (TyVar x) = s n x
  liftShowsPrec s sl n (TyTree x) = liftShowsPrecRec (liftShowsPrec s sl) (liftShowList s sl) s sl n x

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
  traverse f (PtTree x) = PtTree <$> bitransverse traverse f x

instance (Eq a, EqRec pt) => Eq (Pattern pt a) where
  PtVar x == PtVar y = (==) x y
  PtTree x == PtTree y = eqRec x y
  _ == _ = False

instance EqRec pt => Eq1 (Pattern pt) where
  liftEq e (PtVar x) (PtVar y) = e x y
  liftEq e (PtTree x) (PtTree y) = liftEqRec (liftEq e) e x y
  liftEq _ _ _ = False

instance (Ord a, OrdRec pt) => Ord (Pattern pt a) where
  compare (PtVar x) (PtVar y) = compare x y
  compare (PtTree x) (PtTree y) = compareRec x y
  compare (PtVar _) (PtTree _) = LT
  compare (PtTree _) (PtVar _) = GT

instance OrdRec pt => Ord1 (Pattern pt) where
  liftCompare c (PtVar x) (PtVar y) = c x y
  liftCompare c (PtTree x) (PtTree y) = liftCompareRec (liftCompare c) c x y
  liftCompare _ (PtVar _) (PtTree _) = LT
  liftCompare _ (PtTree _) (PtVar _) = GT

instance (Show a, ShowRec pt) => Show (Pattern pt a) where
  showsPrec n (PtVar x) = showsUnaryWith showsPrec "PtVar" n x
  showsPrec n (PtTree x) = showsUnaryWith showsPrecRec "PtTree" n x

instance ShowRec pt => Show1 (Pattern pt) where
  liftShowsPrec s _ n (PtVar x) = s n x
  liftShowsPrec s sl n (PtTree x) = liftShowsPrecRec (liftShowsPrec s sl) (liftShowList s sl) s sl n x

instance (Bound pt, Bitransversable pt) => Applicative (Pattern pt) where
  pure = return
  (<*>) = ap

instance (Bound pt, Bitransversable pt) => Monad (Pattern pt) where
  return = PtVar

  PtVar x >>= f = f x
  PtTree pt >>= f = PtTree (pt >>>= f)

patternToAst :: (Traversable (pt (Pattern pt)), Bitransversable pt) => Pattern pt a -> Ast ty pt tm (AstVar a)
patternToAst = runIdentity . patternToAst' (Identity . APtVar)

patternToAst' :: (Traversable (pt (Pattern pt)), Bitransversable pt) => (a -> Identity b) -> Pattern pt a -> Identity (Ast ty pt tm b)
patternToAst' fV x = patternToAst'' =<< traverse fV x

patternToAst'' :: (Traversable (pt (Pattern pt)), Bitransversable pt) => Pattern pt a -> Identity (Ast ty pt tm a)
patternToAst'' (PtVar x) = Identity (AVar x)
patternToAst'' (PtTree pt) = fmap APattern . bitransverse patternToAst' pure $ pt

astToPattern :: (TripleConstraint1 Traversable ty pt tm, Bitransversable pt) => Ast ty pt tm (AstVar a) -> Maybe (Pattern pt a)
astToPattern = astToPattern' fV
  where
    fV (APtVar x) = Just x
    fV _ = Nothing

astToPattern' :: (TripleConstraint1 Traversable ty pt tm, Bitransversable pt) => (a -> Maybe b) -> Ast ty pt tm a -> Maybe (Pattern pt b)
astToPattern' fV x = astToPattern'' =<< traverse fV x

astToPattern'' :: (TripleConstraint1 Traversable ty pt tm, Bitransversable pt) => Ast ty pt tm a -> Maybe (Pattern pt a)
astToPattern'' (AVar x) = Just (PtVar x)
astToPattern'' (APattern pt) = fmap PtTree . bitransverse astToPattern' pure $ pt
astToPattern'' _ = Nothing

_Pattern :: (Traversable (pt (Pattern pt)), TripleConstraint1 Traversable ty pt tm, Bitransversable pt) => Prism' (Ast ty pt tm (AstVar a)) (Pattern pt a)
_Pattern = prism patternToAst (\x -> note x . astToPattern $ x)

newtype Term ty pt tm a = Term (Ast ty pt tm (AstVar a))

makeWrapped ''Term

deriving instance (Eq a, TripleConstraint Eq ty pt tm (AstVar a)) => Eq (Term ty pt tm a)
deriving instance (Ord a, TripleConstraint Ord ty pt tm (AstVar a)) => Ord (Term ty pt tm a)
deriving instance (Show a, TripleConstraint Show ty pt tm (AstVar a)) => Show (Term ty pt tm a)

instance (TripleConstraint1 Eq1 ty pt tm) => Eq1 (Term ty pt tm) where
  liftEq = $(makeLiftEq ''Term)

instance (TripleConstraint1 Ord1 ty pt tm) => Ord1 (Term ty pt tm) where
  liftCompare = $(makeLiftCompare ''Term)

instance (TripleConstraint1 Show1 ty pt tm) => Show1 (Term ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''Term)

deriving instance (TripleConstraint1 Functor ty pt tm) => Functor (Term ty pt tm)
deriving instance (TripleConstraint1 Foldable ty pt tm) => Foldable (Term ty pt tm)
deriving instance (TripleConstraint1 Traversable ty pt tm) => Traversable (Term ty pt tm)

_TmVar :: Prism' (Term ty pt tm a) a
_TmVar = _Wrapped . _AVar . _ATmVar
