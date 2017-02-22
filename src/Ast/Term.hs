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
    AstVar(..)
  , _ATyVar
  , _APtVar
  , _ATmVar
  , Ast(..)
  , AstEq
  , AstOrd
  , AstShow
  , AstBound
  , AstTransversable
  , _AVar
  , _AType
  , _APattern
  , _ATerm
  , Term(..)
  , _TmVar
  , TmSum(..)
  , _TmNow
  , _TmNext
  , _Type
  , _Pattern
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

import Util

import Ast.Type
import Ast.Pattern

-- AstVar

data AstVar a =
    ATyVar a
  | APtVar a
  | ATmVar a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''AstVar

deriveEq1 ''AstVar
deriveOrd1 ''AstVar
deriveShow1 ''AstVar

-- AstVar

-- TODO with type families, we could add annotations here by just adding some more constraints
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

-- Term

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

-- Type list

data TmSum (f :: [(k1 -> k2 -> k3 -> k4 -> *)]) (g :: k1) (h :: k2) (i :: k3) (a :: k4) where
  TmNext :: TmSum b g h i a -> TmSum (f ': b) g h i a
  TmNow :: f g h i a -> TmSum (f ': b) g h i a

instance (Eq a, Eq1 tm, EqRec (TmSum xs ty pt)) => Eq (TmSum xs ty pt tm a) where
  (==) = eqRec

instance (Eq1 tm, EqRec (TmSum xs ty pt)) => Eq1 (TmSum xs ty pt tm) where
  liftEq = liftEq1Rec

instance (Ord a, Ord1 tm, OrdRec (TmSum xs ty pt)) => Ord (TmSum xs ty pt tm a) where
  compare = compareRec

instance (Ord1 tm, OrdRec (TmSum xs ty pt)) => Ord1 (TmSum xs ty pt tm) where
  liftCompare = liftCompare1Rec

instance (Show a, Show1 tm, ShowRec (TmSum xs ty pt)) => Show (TmSum xs ty pt tm a) where
  showsPrec = showsPrecRec

instance (Show1 tm, ShowRec (TmSum xs ty pt)) => Show1 (TmSum xs ty pt tm) where
  liftShowsPrec = liftShowsPrec1Rec

instance (Traversable tm, Bitransversable (TmSum xs ty pt)) => Functor (TmSum xs ty pt tm) where
  fmap = fmapDefault

instance (Traversable tm, Bitransversable (TmSum xs ty pt)) => Foldable (TmSum xs ty pt tm) where
  foldMap = foldMapDefault

instance (Traversable tm, Bitransversable (TmSum xs ty pt)) => Traversable (TmSum xs ty pt tm) where
  traverse = traverseDefault

_TmNext :: Prism' (TmSum (f ': b) ty pt tm a) (TmSum b ty pt tm a)
_TmNext = prism TmNext $ \x -> case x of
  TmNext y -> Right y
  _ -> Left x

_TmNow :: Prism' (TmSum (f ': b) ty pt tm a) (f ty pt tm a)
_TmNow = prism TmNow $ \x -> case x of
  TmNow y -> Right y
  _ -> Left x

instance Bound (TmSum '[] ty pt) where
  _ >>>= _ = error "cannot use Bound with an empty list"

instance (Bound (x ty pt), Bound (TmSum xs ty pt)) => Bound (TmSum (x ': xs) ty pt) where
  TmNow a >>>= f = TmNow (a >>>= f)
  TmNext n >>>= f = TmNext (n >>>= f)

instance Bitransversable (TmSum '[] ty pt) where
  bitransverse _ _ = error "cannot use Bitransversable with an empty list"

instance (Bitransversable (x ty pt), Bitransversable (TmSum xs ty pt)) => Bitransversable (TmSum (x ': xs) ty pt) where
  bitransverse fT fL (TmNow a) = TmNow <$> bitransverse fT fL a
  bitransverse fT fL (TmNext n) = TmNext <$> bitransverse fT fL n

instance EqRec (TmSum '[] ty pt) where
  liftEqRec _ _ _ _ = True

instance (EqRec (x ty pt), EqRec (TmSum xs ty pt)) => EqRec (TmSum (x ': xs) ty pt) where
  liftEqRec eR e (TmNow a1) (TmNow a2) =
    liftEqRec eR e a1 a2
  liftEqRec eR e (TmNext n1) (TmNext n2) =
    liftEqRec eR e n1 n2
  liftEqRec _ _ _ _ =
    False

instance OrdRec (TmSum '[] ty pt) where
  liftCompareRec _ _ _ _ = EQ

instance (EqRec (x ty pt), EqRec (TmSum xs ty pt), OrdRec (x ty pt), OrdRec (TmSum xs ty pt)) => OrdRec (TmSum (x ': xs) ty pt) where
  liftCompareRec cR c (TmNow a1) (TmNow a2) =
    liftCompareRec cR c a1 a2
  liftCompareRec _ _ (TmNow _) _ =
    LT
  liftCompareRec _ _ _ (TmNow _) =
    GT
  liftCompareRec cR c (TmNext n1) (TmNext n2) =
    liftCompareRec cR c n1 n2

instance ShowRec (TmSum '[] ty pt) where
  liftShowsPrecRec _ _ _ _ _ _ = id

instance (ShowRec (x ty pt), ShowRec (TmSum xs ty pt)) => ShowRec (TmSum (x ': xs) ty pt) where
  liftShowsPrecRec sR slR s sl m (TmNow a) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TmSum" m a
  liftShowsPrecRec sR slR s sl m (TmNext n) =
    liftShowsPrecRec sR slR s sl m n

-- Prisms

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
