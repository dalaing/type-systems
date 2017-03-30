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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Ast.Type (
    TyAstVar(..)
  , _TyAstKiVar
  , _TyAstTyVar
  , TyAst(..)
  , TyAstEq
  , TyAstOrd
  , TyAstShow
  , TyAstBound
  , TyAstTransversable
  , _TyAstVar
  , _TyAstKind
  , _TyAstType
  , Type(..)
  , _TyVar
  , TySum(..)
  , _TyNow
  , _TyNext
  , _TyKind
  , scopeAppTy
  , abstractTy
  , instantiateTy
  ) where

import Control.Monad (ap)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)
import Data.Traversable (fmapDefault, foldMapDefault)
import GHC.Exts (Constraint)

import Bound (Bound(..), Scope, Var(..), abstract, instantiate, toScope, fromScope)
import Control.Error (note)
import Control.Lens (review)
import Control.Lens.Iso (Iso', iso, from, mapping)
import Control.Lens.Prism (Prism', prism)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms, makeWrapped)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1, makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Kind
import Data.Bitransversable
import Data.Functor.Rec

data TyAstVar a =
    TyAstKiVar a
  | TyAstTyVar a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyAstVar

deriveEq1 ''TyAstVar
deriveOrd1 ''TyAstVar
deriveShow1 ''TyAstVar

data TyAst (ki :: (* -> *) -> * -> *) ty a =
    TyAstVar a
  | TyAstKind (ki (TyAst ki ty ) a)
  | TyAstType (ty ki (TyAst ki ty) a)

makePrisms ''TyAst

type TyAstConstraint (k :: ((* -> *) -> * -> *) -> Constraint) ki ty = (k ki, k (ty ki))
type TyAstTransversable ki ty = TyAstConstraint (Bitransversable) ki ty
type TyAstBound ki ty = TyAstConstraint Bound ki ty
type TyAstEq ki ty = TyAstConstraint EqRec ki ty
type TyAstOrd ki ty = TyAstConstraint OrdRec ki ty
type TyAstShow ki ty = TyAstConstraint ShowRec ki ty

instance (Eq a, TyAstEq ki ty) => Eq (TyAst ki ty a) where
  TyAstVar x == TyAstVar y = x == y
  TyAstKind x == TyAstKind y = eqRec x y
  TyAstType x == TyAstType y = eqRec x y
  _ == _ = False

instance TyAstEq ki ty => Eq1 (TyAst ki ty) where
  liftEq e (TyAstVar x) (TyAstVar y) = e x y
  liftEq e (TyAstKind x) (TyAstKind y) = liftEq1Rec e x y
  liftEq e (TyAstType x) (TyAstType y) = liftEq1Rec e x y
  liftEq _ _ _ = False

instance (Ord a, TyAstOrd ki ty) => Ord (TyAst ki ty a) where
  compare (TyAstVar x) (TyAstVar y) = compare x y
  compare (TyAstVar _) _ = LT
  compare _ (TyAstVar _) = GT
  compare (TyAstKind x) (TyAstKind y) = compareRec x y
  compare (TyAstKind _) _ = LT
  compare _ (TyAstKind _) = GT
  compare (TyAstType x) (TyAstType y) = compareRec x y

instance TyAstOrd ki ty => Ord1 (TyAst ki ty) where
  liftCompare c (TyAstVar x) (TyAstVar y) = c x y
  liftCompare _ (TyAstVar _) _ = LT
  liftCompare _ _ (TyAstVar _) = GT
  liftCompare c (TyAstKind x) (TyAstKind y) = liftCompare1Rec c x y
  liftCompare _ (TyAstKind _) _ = LT
  liftCompare _ _ (TyAstKind _) = GT
  liftCompare c (TyAstType x) (TyAstType y) = liftCompare1Rec c x y

instance (Show a, TyAstShow ki ty) => Show (TyAst ki ty a) where
  showsPrec n (TyAstVar x) = showsUnaryWith showsPrec "TyAstVar" n x
  showsPrec n (TyAstKind x) = showsUnaryWith showsPrecRec "TyAstKind" n x
  showsPrec n (TyAstType x) = showsUnaryWith showsPrecRec "TyAstType" n x

instance TyAstShow ki ty => Show1 (TyAst ki ty) where
  liftShowsPrec s _ n (TyAstVar x) = s n x
  liftShowsPrec s sl n (TyAstKind x) = liftShowsPrec1Rec s sl n x
  liftShowsPrec s sl n (TyAstType x) = liftShowsPrec1Rec s sl n x

instance TyAstTransversable ki ty => Functor (TyAst ki ty) where
  fmap = fmapDefault

instance TyAstTransversable ki ty => Foldable (TyAst ki ty) where
  foldMap = foldMapDefault

instance TyAstTransversable ki ty => Traversable (TyAst ki ty) where
  traverse f (TyAstVar x) = TyAstVar <$> f x
  traverse f (TyAstKind x) = TyAstKind <$> traverseDefault f x
  traverse f (TyAstType x) = TyAstType <$> traverseDefault f x

instance (TyAstTransversable ki ty, TyAstBound ki ty) => Applicative (TyAst ki ty) where
  pure = return
  (<*>) = ap

instance (TyAstTransversable ki ty, TyAstBound ki ty) => Monad (TyAst ki ty) where
  return = TyAstVar

  TyAstVar x >>= f = f x
  TyAstKind ki >>= f = TyAstKind (ki >>>= f)
  TyAstType ty >>= f = TyAstType (ty >>>= f)

newtype Type ki ty a = Type (TyAst ki ty (TyAstVar a))
  deriving (Eq, Ord, Show)

makeWrapped ''Type

instance TyAstEq ki ty => Eq1 (Type ki ty) where
  liftEq = $(makeLiftEq ''Type)

instance TyAstOrd ki ty => Ord1 (Type ki ty) where
  liftCompare = $(makeLiftCompare ''Type)

instance TyAstShow ki ty => Show1 (Type ki ty) where
  liftShowsPrec = $(makeLiftShowsPrec ''Type)

deriving instance TyAstTransversable ki ty => Functor (Type ki ty)
deriving instance TyAstTransversable ki ty => Foldable (Type ki ty)
deriving instance TyAstTransversable ki ty => Traversable (Type ki ty)

_TyVar :: Prism' (Type ki ty a) a
_TyVar = _Wrapped . _TyAstVar . _TyAstTyVar

data TySum (tys :: [(k1 -> k2 -> k3 -> *)]) (ki :: k1) (h :: k2) (a :: k3) where
  TyNext :: TySum tys ki h a -> TySum (ty ': tys) ki h a
  TyNow :: ty ki h a -> TySum (ty ': tys) ki h a

instance (Eq a, Eq1 h, EqRec (TySum tys ki)) => Eq (TySum tys ki h a) where
  (==) = eqRec

instance (Eq1 h, EqRec (TySum tys ki)) => Eq1 (TySum tys ki h) where
  liftEq = liftEq1Rec

instance (Ord a, Ord1 h, OrdRec (TySum tys ki)) => Ord (TySum tys ki h a) where
  compare = compareRec

instance (Ord1 h, OrdRec (TySum tys ki)) => Ord1 (TySum tys ki h) where
  liftCompare = liftCompare1Rec

instance (Show a, Show1 h, ShowRec (TySum tys ki)) => Show (TySum tys ki h a) where
  showsPrec = showsPrecRec

instance (Show1 h, ShowRec (TySum tys ki)) => Show1 (TySum tys ki h) where
  liftShowsPrec = liftShowsPrec1Rec

instance (Traversable h, Bitransversable (TySum tys ki)) => Functor (TySum tys ki h) where
  fmap = fmapDefault

instance (Traversable h, Bitransversable (TySum tys ki)) => Foldable (TySum tys ki h) where
  foldMap = foldMapDefault

instance (Traversable h, Bitransversable (TySum tys ki)) => Traversable (TySum tys ki h) where
  traverse = traverseDefault

_TyNext :: Prism' (TySum (ty ': tys) ki h a) (TySum tys ki h a)
_TyNext = prism TyNext $ \x -> case x of
  TyNext y -> Right y
  _ -> Left x

_TyNow :: Prism' (TySum (ty ': tys) ki h a) (ty ki h a)
_TyNow = prism TyNow $ \x -> case x of
  TyNow y -> Right y
  _ -> Left x

instance Bound (TySum '[] ki) where
  _ >>>= _ = error "cannot use Bound with an empty list"

instance (Bound (x ki), Bound (TySum xs ki)) => Bound (TySum (x ': xs) ki) where
  TyNow a >>>= f = TyNow (a >>>= f)
  TyNext n >>>= f = TyNext (n >>>= f)

instance Bitransversable (TySum '[] ki) where
  bitransverse _ _ = error "cannot use Bitransversable with an empty list"

instance (Bitransversable (x ki), Bitransversable (TySum xs ki)) => Bitransversable (TySum (x ': xs) ki) where
  bitransverse fT fL (TyNow a) = TyNow <$> bitransverse fT fL a
  bitransverse fT fL (TyNext n) = TyNext <$> bitransverse fT fL n

instance EqRec (TySum '[] ki) where
  liftEqRec _ _ _ _ = True

instance (EqRec (x ki), EqRec (TySum xs ki)) => EqRec (TySum (x ': xs) ki) where
  liftEqRec eR e (TyNow a1) (TyNow a2) =
    liftEqRec eR e a1 a2
  liftEqRec eR e (TyNext n1) (TyNext n2) =
    liftEqRec eR e n1 n2
  liftEqRec _ _ _ _ =
    False

instance OrdRec (TySum '[] ki) where
  liftCompareRec _ _ _ _ = EQ

instance (EqRec (x ki), EqRec (TySum xs ki), OrdRec (x ki), OrdRec (TySum xs ki)) => OrdRec (TySum (x ': xs) ki) where
  liftCompareRec cR c (TyNow a1) (TyNow a2) =
    liftCompareRec cR c a1 a2
  liftCompareRec _ _ (TyNow _) _ =
    LT
  liftCompareRec _ _ _ (TyNow _) =
    GT
  liftCompareRec cR c (TyNext n1) (TyNext n2) =
    liftCompareRec cR c n1 n2

instance ShowRec (TySum '[] ki) where
  liftShowsPrecRec _ _ _ _ _ _ = id

instance (ShowRec (x ki), ShowRec (TySum xs ki)) => ShowRec (TySum (x ': xs) ki) where
  liftShowsPrecRec sR slR s sl m (TyNow a) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TySum" m a
  liftShowsPrecRec sR slR s sl m (TyNext n) =
    liftShowsPrecRec sR slR s sl m n

kindToAst :: Bitransversable ki => Kind ki a -> TyAst ki ty (TyAstVar a)
kindToAst = runIdentity . kindToAst' (Identity . TyAstTyVar)

kindToAst' :: Bitransversable ki => (a -> Identity b) -> Kind ki a -> Identity (TyAst ki ty b)
kindToAst' fV x = kindToAst'' =<< traverse fV x

kindToAst'' :: Bitransversable ki => Kind ki a -> Identity (TyAst ki ty a)
kindToAst'' (KiVar x) = Identity (TyAstVar x)
kindToAst'' (KiTree ty) = fmap TyAstKind . bitransverse kindToAst' pure $ ty

astToKind :: TyAstTransversable ki ty => TyAst ki ty (TyAstVar a) -> Maybe (Kind ki a)
astToKind = astToKind' fV
  where
    fV (TyAstKiVar x) = Just x
    fV _ = Nothing

astToKind' :: TyAstTransversable ki ty => (a -> Maybe b) -> TyAst ki ty a -> Maybe (Kind ki b)
astToKind' fV x = astToKind'' =<< traverse fV x

astToKind'' :: TyAstTransversable ki ty => TyAst ki ty a -> Maybe (Kind ki a)
astToKind'' (TyAstVar x) = Just (KiVar x)
astToKind'' (TyAstKind ty) = fmap KiTree . bitransverse astToKind' pure $ ty
astToKind'' _ = Nothing

_TyKind :: TyAstTransversable ki ty => Prism' (TyAst ki ty (TyAstVar a)) (Kind ki a)
_TyKind = prism kindToAst (\x -> note x . astToKind $ x)

scopeTyVar :: Iso' (Var b (TyAstVar f)) (TyAstVar (Var b f))
scopeTyVar = iso there back
  where
    there (B b) = TyAstTyVar (B b)
    there (F (TyAstKiVar f)) = TyAstKiVar (F f)
    there (F (TyAstTyVar f)) = TyAstTyVar (F f)

    back (TyAstKiVar (B b)) = B b
    back (TyAstKiVar (F f)) = F (TyAstKiVar f)
    back (TyAstTyVar (B b)) = B b
    back (TyAstTyVar (F f)) = F (TyAstTyVar f)

scopeAppTy :: (TyAstBound ki ty, TyAstTransversable ki ty)
           => (forall x. Type ki ty x -> Type ki ty x)
           -> Scope b (TyAst ki ty) (TyAstVar a)
           -> Scope b (TyAst ki ty) (TyAstVar a)
scopeAppTy fn =
  toScope .
  review (mapping scopeTyVar . _Unwrapped) .
  fn .
  review (_Wrapped . mapping (from scopeTyVar)) .
  fromScope

abstractTy :: (Eq a, TyAstBound ki ty , TyAstTransversable ki ty)
           => a
           -> Type ki ty a
           -> Scope () (TyAst ki ty) (TyAstVar a)
abstractTy v (Type ty) = abstract f ty
  where
    f (TyAstTyVar x)
      | v == x = Just ()
      | otherwise = Nothing
    f _ = Nothing

instantiateTy :: (TyAstBound ki ty, TyAstTransversable ki ty)
              => Type ki ty a
              -> Scope () (TyAst ki ty) (TyAstVar a)
              -> Type ki ty a
instantiateTy (Type ty) s = Type . instantiate f $ s
  where
    f _ = ty


