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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Ast.Term (
    TmAstVar(..)
  , _TmAstKiVar
  , _TmAstTyVar
  , _TmAstPtVar
  , _TmAstTmVar
  , TmAst(..)
  , TmAstEq
  , TmAstOrd
  , TmAstShow
  , TmAstBound
  , TmAstTransversable
  , _TmAstVar
  , _TmAstKind
  , _TmAstType
  , _TmAstPattern
  , _TmAstTerm
  , Term(..)
  , _TmVar
  , TmSum(..)
  , _TmNow
  , _TmNext
  , _TmKind
  , _TmType
  , _TmPattern
  ) where

import Control.Monad (ap)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)
import Data.Traversable (fmapDefault, foldMapDefault)
import GHC.Exts (Constraint)

import Control.Error (note)
import Control.Lens.Prism (Prism', prism)
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms, makeWrapped)
import Bound (Bound(..))
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1, makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Data.Bitransversable
import Data.Functor.Rec

import Ast.Kind
import Ast.Type
import Ast.Pattern

-- AstVar

data TmAstVar a =
    TmAstKiVar a
  | TmAstTyVar a
  | TmAstPtVar a
  | TmAstTmVar a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmAstVar

deriveEq1 ''TmAstVar
deriveOrd1 ''TmAstVar
deriveShow1 ''TmAstVar

-- AstVar

-- TODO with type families, we could add annotations here by just adding some more constraints
data TmAst (ki :: (* -> *) -> * -> *) ty pt tm a =
    TmAstVar a
  | TmAstKind (ki (TmAst ki ty pt tm) a)
  | TmAstType (ty ki (TmAst ki ty pt tm) a)
  | TmAstPattern (pt (TmAst ki ty pt tm) a)
  | TmAstTerm (tm ki ty pt (TmAst ki ty pt tm) a)

makePrisms ''TmAst

type TmAstConstraint (k :: ((* -> *) -> * -> *) -> Constraint) ki ty pt tm = (k ki, k (ty ki), k pt, k (tm ki ty pt))
type TmAstTransversable ki ty pt tm = TmAstConstraint (Bitransversable) ki ty pt tm
type TmAstBound ki ty pt tm = TmAstConstraint Bound ki ty pt tm
type TmAstEq ki ty pt tm = TmAstConstraint EqRec ki ty pt tm
type TmAstOrd ki ty pt tm = TmAstConstraint OrdRec ki ty pt tm
type TmAstShow ki ty pt tm = TmAstConstraint ShowRec ki ty pt tm

instance (Eq a, TmAstEq ki ty pt tm) => Eq (TmAst ki ty pt tm a) where
  TmAstVar x == TmAstVar y = x == y
  TmAstKind x == TmAstKind y = eqRec x y
  TmAstType x == TmAstType y = eqRec x y
  TmAstPattern x == TmAstPattern y = eqRec x y
  TmAstTerm x == TmAstTerm y = eqRec x y
  _ == _ = False

instance TmAstEq ki ty pt tm => Eq1 (TmAst ki ty pt tm) where
  liftEq e (TmAstVar x) (TmAstVar y) = e x y
  liftEq e (TmAstKind x) (TmAstKind y) = liftEq1Rec e x y
  liftEq e (TmAstType x) (TmAstType y) = liftEq1Rec e x y
  liftEq e (TmAstPattern x) (TmAstPattern y) = liftEq1Rec e x y
  liftEq e (TmAstTerm x) (TmAstTerm y) = liftEq1Rec e x y
  liftEq _ _ _ = False

instance (Ord a, TmAstOrd ki ty pt tm) => Ord (TmAst ki ty pt tm a) where
  compare (TmAstVar x) (TmAstVar y) = compare x y
  compare (TmAstVar _) _ = LT
  compare _ (TmAstVar _) = GT
  compare (TmAstKind x) (TmAstKind y) = compareRec x y
  compare (TmAstKind _) _ = LT
  compare _ (TmAstKind _) = GT
  compare (TmAstType x) (TmAstType y) = compareRec x y
  compare (TmAstType _) _ = LT
  compare _ (TmAstType _) = GT
  compare (TmAstPattern x) (TmAstPattern y) = compareRec x y
  compare (TmAstPattern _) _ = LT
  compare _ (TmAstPattern _) = GT
  compare (TmAstTerm x) (TmAstTerm y) = compareRec x y

instance TmAstOrd ki ty pt tm => Ord1 (TmAst ki ty pt tm) where
  liftCompare c (TmAstVar x) (TmAstVar y) = c x y
  liftCompare _ (TmAstVar _) _ = LT
  liftCompare _ _ (TmAstVar _) = GT
  liftCompare c (TmAstKind x) (TmAstKind y) = liftCompare1Rec c x y
  liftCompare _ (TmAstKind _) _ = LT
  liftCompare _ _ (TmAstKind _) = GT
  liftCompare c (TmAstType x) (TmAstType y) = liftCompare1Rec c x y
  liftCompare _ (TmAstType _) _ = LT
  liftCompare _ _ (TmAstType _) = GT
  liftCompare c (TmAstPattern x) (TmAstPattern y) = liftCompare1Rec c x y
  liftCompare _ (TmAstPattern _) _ = LT
  liftCompare _ _ (TmAstPattern _) = GT
  liftCompare c (TmAstTerm x) (TmAstTerm y) = liftCompare1Rec c x y

instance (Show a, TmAstShow ki ty pt tm) => Show (TmAst ki ty pt tm a) where
  showsPrec n (TmAstVar x) = showsUnaryWith showsPrec "TmAstVar" n x
  showsPrec n (TmAstKind x) = showsUnaryWith showsPrecRec "TmAstKind" n x
  showsPrec n (TmAstType x) = showsUnaryWith showsPrecRec "TmAstType" n x
  showsPrec n (TmAstPattern x) = showsUnaryWith showsPrecRec "TmAstPattern" n x
  showsPrec n (TmAstTerm x) = showsUnaryWith showsPrecRec "TmAstTerm" n x

instance TmAstShow ki ty pt tm => Show1 (TmAst ki ty pt tm) where
  liftShowsPrec s _ n (TmAstVar x) = s n x
  liftShowsPrec s sl n (TmAstKind x) = liftShowsPrec1Rec s sl n x
  liftShowsPrec s sl n (TmAstType x) = liftShowsPrec1Rec s sl n x
  liftShowsPrec s sl n (TmAstPattern x) = liftShowsPrec1Rec s sl n x
  liftShowsPrec s sl n (TmAstTerm x) = liftShowsPrec1Rec s sl n x

instance TmAstTransversable ki ty pt tm => Functor (TmAst ki ty pt tm) where
  fmap = fmapDefault

instance TmAstTransversable ki ty pt tm => Foldable (TmAst ki ty pt tm) where
  foldMap = foldMapDefault

instance TmAstTransversable ki ty pt tm => Traversable (TmAst ki ty pt tm) where
  traverse f (TmAstVar x) = TmAstVar <$> f x
  traverse f (TmAstKind x) = TmAstKind <$> traverseDefault f x
  traverse f (TmAstType x) = TmAstType <$> traverseDefault f x
  traverse f (TmAstPattern x) = TmAstPattern <$> traverseDefault f x
  traverse f (TmAstTerm x) = TmAstTerm <$> traverseDefault f x

instance (TmAstTransversable ki ty pt tm, TmAstBound ki ty pt tm) => Applicative (TmAst ki ty pt tm) where
  pure = return
  (<*>) = ap

instance (TmAstTransversable ki ty pt tm, TmAstBound ki ty pt tm) => Monad (TmAst ki ty pt tm) where
  return = TmAstVar

  TmAstVar x >>= f = f x
  TmAstKind ki >>= f = TmAstKind (ki >>>= f)
  TmAstType ty >>= f = TmAstType (ty >>>= f)
  TmAstPattern pt >>= f = TmAstPattern (pt >>>= f)
  TmAstTerm tm >>= f = TmAstTerm (tm >>>= f)

-- Term

newtype Term ki ty pt tm a = Term (TmAst ki ty pt tm (TmAstVar a))
  deriving (Eq, Ord, Show)

makeWrapped ''Term

instance TmAstEq ki ty pt tm => Eq1 (Term ki ty pt tm) where
  liftEq = $(makeLiftEq ''Term)

instance TmAstOrd ki ty pt tm => Ord1 (Term ki ty pt tm) where
  liftCompare = $(makeLiftCompare ''Term)

instance TmAstShow ki ty pt tm => Show1 (Term ki ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''Term)

deriving instance TmAstTransversable ki ty pt tm => Functor (Term ki ty pt tm)
deriving instance TmAstTransversable ki ty pt tm => Foldable (Term ki ty pt tm)
deriving instance TmAstTransversable ki ty pt tm => Traversable (Term ki ty pt tm)

_TmVar :: Prism' (Term ki ty pt tm a) a
_TmVar = _Wrapped . _TmAstVar . _TmAstTmVar

-- Type list

data TmSum (f :: [(k1 -> k2 -> k3 -> k4 -> k5 -> *)]) (g :: k1) (h :: k2) (i :: k3) (j :: k4) (a :: k5) where
  TmNext :: TmSum b g h i j a -> TmSum (f ': b) g h i j a
  TmNow :: f g h i j a -> TmSum (f ': b) g h i j a

instance (Eq a, Eq1 tm, EqRec (TmSum xs ki ty pt)) => Eq (TmSum xs ki ty pt tm a) where
  (==) = eqRec

instance (Eq1 tm, EqRec (TmSum xs ki ty pt)) => Eq1 (TmSum xs ki ty pt tm) where
  liftEq = liftEq1Rec

instance (Ord a, Ord1 tm, OrdRec (TmSum xs ki ty pt)) => Ord (TmSum xs ki ty pt tm a) where
  compare = compareRec

instance (Ord1 tm, OrdRec (TmSum xs ki ty pt)) => Ord1 (TmSum xs ki ty pt tm) where
  liftCompare = liftCompare1Rec

instance (Show a, Show1 tm, ShowRec (TmSum xs ki ty pt)) => Show (TmSum xs ki ty pt tm a) where
  showsPrec = showsPrecRec

instance (Show1 tm, ShowRec (TmSum xs ki ty pt)) => Show1 (TmSum xs ki ty pt tm) where
  liftShowsPrec = liftShowsPrec1Rec

instance (Traversable tm, Bitransversable (TmSum xs ki ty pt)) => Functor (TmSum xs ki ty pt tm) where
  fmap = fmapDefault

instance (Traversable tm, Bitransversable (TmSum xs ki ty pt)) => Foldable (TmSum xs ki ty pt tm) where
  foldMap = foldMapDefault

instance (Traversable tm, Bitransversable (TmSum xs ki ty pt)) => Traversable (TmSum xs ki ty pt tm) where
  traverse = traverseDefault

_TmNext :: Prism' (TmSum (f ': b) ki ty pt tm a) (TmSum b ki ty pt tm a)
_TmNext = prism TmNext $ \x -> case x of
  TmNext y -> Right y
  _ -> Left x

_TmNow :: Prism' (TmSum (f ': b) ki ty pt tm a) (f ki ty pt tm a)
_TmNow = prism TmNow $ \x -> case x of
  TmNow y -> Right y
  _ -> Left x

instance Bound (TmSum '[] ki ty pt) where
  _ >>>= _ = error "cannot use Bound with an empty list"

instance (Bound (x ki ty pt), Bound (TmSum xs ki ty pt)) => Bound (TmSum (x ': xs) ki ty pt) where
  TmNow a >>>= f = TmNow (a >>>= f)
  TmNext n >>>= f = TmNext (n >>>= f)

instance Bitransversable (TmSum '[] ki ty pt) where
  bitransverse _ _ = error "cannot use Bitransversable with an empty list"

instance (Bitransversable (x ki ty pt), Bitransversable (TmSum xs ki ty pt)) => Bitransversable (TmSum (x ': xs) ki ty pt) where
  bitransverse fT fL (TmNow a) = TmNow <$> bitransverse fT fL a
  bitransverse fT fL (TmNext n) = TmNext <$> bitransverse fT fL n

instance EqRec (TmSum '[] ki ty pt) where
  liftEqRec _ _ _ _ = True

instance (EqRec (x ki ty pt), EqRec (TmSum xs ki ty pt)) => EqRec (TmSum (x ': xs) ki ty pt) where
  liftEqRec eR e (TmNow a1) (TmNow a2) =
    liftEqRec eR e a1 a2
  liftEqRec eR e (TmNext n1) (TmNext n2) =
    liftEqRec eR e n1 n2
  liftEqRec _ _ _ _ =
    False

instance OrdRec (TmSum '[] ki ty pt) where
  liftCompareRec _ _ _ _ = EQ

instance (EqRec (x ki ty pt), EqRec (TmSum xs ki ty pt), OrdRec (x ki ty pt), OrdRec (TmSum xs ki ty pt)) => OrdRec (TmSum (x ': xs) ki ty pt) where
  liftCompareRec cR c (TmNow a1) (TmNow a2) =
    liftCompareRec cR c a1 a2
  liftCompareRec _ _ (TmNow _) _ =
    LT
  liftCompareRec _ _ _ (TmNow _) =
    GT
  liftCompareRec cR c (TmNext n1) (TmNext n2) =
    liftCompareRec cR c n1 n2

instance ShowRec (TmSum '[] ki ty pt) where
  liftShowsPrecRec _ _ _ _ _ _ = id

instance (ShowRec (x ki ty pt), ShowRec (TmSum xs ki ty pt)) => ShowRec (TmSum (x ': xs) ki ty pt) where
  liftShowsPrecRec sR slR s sl m (TmNow a) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TmSum" m a
  liftShowsPrecRec sR slR s sl m (TmNext n) =
    liftShowsPrecRec sR slR s sl m n

-- Prisms

kindToAst :: Bitransversable ki => Kind ki a -> TmAst ki ty pt tm (TmAstVar a)
kindToAst = runIdentity . kindToAst' (Identity . TmAstTyVar)

kindToAst' :: Bitransversable ki => (a -> Identity b) -> Kind ki a -> Identity (TmAst ki ty pt tm b)
kindToAst' fV x = kindToAst'' =<< traverse fV x

kindToAst'' :: Bitransversable ki => Kind ki a -> Identity (TmAst ki ty pt tm a)
kindToAst'' (KiVar x) = Identity (TmAstVar x)
kindToAst'' (KiTree ty) = fmap TmAstKind . bitransverse kindToAst' pure $ ty

astToKind :: TmAstTransversable ki ty pt tm => TmAst ki ty pt tm (TmAstVar a) -> Maybe (Kind ki a)
astToKind = astToKind' fV
  where
    fV (TmAstKiVar x) = Just x
    fV _ = Nothing

astToKind' :: TmAstTransversable ki ty pt tm => (a -> Maybe b) -> TmAst ki ty pt tm a -> Maybe (Kind ki b)
astToKind' fV x = astToKind'' =<< traverse fV x

astToKind'' :: TmAstTransversable ki ty pt tm => TmAst ki ty pt tm a -> Maybe (Kind ki a)
astToKind'' (TmAstVar x) = Just (KiVar x)
astToKind'' (TmAstKind ty) = fmap KiTree . bitransverse astToKind' pure $ ty
astToKind'' _ = Nothing

_TmKind :: TmAstTransversable ki ty pt tm => Prism' (TmAst ki ty pt tm (TmAstVar a)) (Kind ki a)
_TmKind = prism kindToAst (\x -> note x . astToKind $ x)

typeToAst :: Bitransversable (ty ki) => Type ki ty a -> TmAst ki ty pt tm (TmAstVar a)
typeToAst = runIdentity . typeToAst' (Identity . TmAstTyVar)

typeToAst' :: Bitransversable (ty ki) => (a -> Identity b) -> Type ki ty a -> Identity (TmAst ki ty pt tm b)
typeToAst' fV x = typeToAst'' =<< traverse fV x

typeToAst'' :: Bitransversable (ty ki) => Type ki ty a -> Identity (TmAst ki ty pt tm a)
typeToAst'' (TyVar x) = Identity (TmAstVar x)
typeToAst'' (TyTree ty) = fmap TmAstType . bitransverse typeToAst' pure $ ty

astToType :: TmAstTransversable ki ty pt tm => TmAst ki ty pt tm (TmAstVar a) -> Maybe (Type ki ty a)
astToType = astToType' fV
  where
    fV (TmAstTyVar x) = Just x
    fV _ = Nothing

astToType' :: TmAstTransversable ki ty pt tm => (a -> Maybe b) -> TmAst ki ty pt tm a -> Maybe (Type ki ty b)
astToType' fV x = astToType'' =<< traverse fV x

astToType'' :: TmAstTransversable ki ty pt tm => TmAst ki ty pt tm a -> Maybe (Type ki ty a)
astToType'' (TmAstVar x) = Just (TyVar x)
astToType'' (TmAstType ty) = fmap TyTree . bitransverse astToType' pure $ ty
astToType'' _ = Nothing

_TmType :: TmAstTransversable ki ty pt tm => Prism' (TmAst ki ty pt tm (TmAstVar a)) (Type ki ty a)
_TmType = prism typeToAst (\x -> note x . astToType $ x)

patternToAst :: Bitransversable pt => Pattern pt a -> TmAst ki ty pt tm (TmAstVar a)
patternToAst = runIdentity . patternToAst' (Identity . TmAstPtVar)

patternToAst' :: Bitransversable pt => (a -> Identity b) -> Pattern pt a -> Identity (TmAst ki ty pt tm b)
patternToAst' fV x = patternToAst'' =<< traverse fV x

patternToAst'' :: Bitransversable pt => Pattern pt a -> Identity (TmAst ki ty pt tm a)
patternToAst'' (PtVar x) = pure (TmAstVar x)
patternToAst'' (PtTree pt) = fmap TmAstPattern . bitransverse patternToAst' pure $ pt

astToPattern :: TmAstTransversable ki ty pt tm => TmAst ki ty pt tm (TmAstVar a) -> Maybe (Pattern pt a)
astToPattern = astToPattern' fV
  where
    fV (TmAstPtVar x) = Just x
    fV _ = Nothing

astToPattern' :: TmAstTransversable ki ty pt tm => (a -> Maybe b) -> TmAst ki ty pt tm a -> Maybe (Pattern pt b)
astToPattern' fV x = astToPattern'' =<< traverse fV x

astToPattern'' :: TmAstTransversable ki ty pt tm => TmAst ki ty pt tm a -> Maybe (Pattern pt a)
astToPattern'' (TmAstVar x) = pure (PtVar x)
astToPattern'' (TmAstPattern pt) = fmap PtTree . bitransverse astToPattern' pure $ pt
astToPattern'' _ = Nothing

_TmPattern :: TmAstTransversable ki ty pt tm => Prism' (TmAst ki ty pt tm (TmAstVar a)) (Pattern pt a)
_TmPattern = prism patternToAst (\x -> note x . astToPattern $ x)
