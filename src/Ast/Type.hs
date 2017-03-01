{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Ast.Type (
    Type(..)
  , _TyVar
  , _TyTree
  , TySum(..)
  , _TyNow
  , _TyNext
  ) where

import Control.Monad (ap)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)
import Data.Traversable (fmapDefault, foldMapDefault)

import Bound (Bound(..))
import Control.Lens.Prism (Prism', prism)
import Control.Lens.TH (makePrisms)

import Data.Bitransversable
import Data.Functor.Rec

data Type (ki :: * -> *) ty a =
    TyVar a
  | TyTree (ty ki (Type ki ty) a)

makePrisms ''Type

instance Bitransversable (ty ki) => Functor (Type ki ty) where
  fmap = fmapDefault

instance Bitransversable (ty ki) => Foldable (Type ki ty) where
  foldMap = foldMapDefault

instance Bitransversable (ty ki) => Traversable (Type ki ty) where
  traverse f (TyVar x) = TyVar <$> f x
  traverse f (TyTree x) = TyTree <$> traverseDefault f x

instance (Eq a, EqRec (ty ki)) => Eq (Type ki ty a) where
  TyVar x == TyVar y = (==) x y
  TyTree x == TyTree y = eqRec x y
  _ == _ = False

instance EqRec (ty ki) => Eq1 (Type ki ty) where
  liftEq e (TyVar x) (TyVar y) = e x y
  liftEq e (TyTree x) (TyTree y) = liftEq1Rec e x y
  liftEq _ _ _ = False

instance (Ord a, OrdRec (ty ki)) => Ord (Type ki ty a) where
  compare (TyVar x) (TyVar y) = compare x y
  compare (TyVar _) _ = LT
  compare _ (TyVar _) = GT
  compare (TyTree x) (TyTree y) = compareRec x y

instance OrdRec (ty ki) => Ord1 (Type ki ty) where
  liftCompare c (TyVar x) (TyVar y) = c x y
  liftCompare _ (TyVar _) _ = LT
  liftCompare _ _ (TyVar _) = GT
  liftCompare c (TyTree x) (TyTree y) = liftCompare1Rec c x y

instance (Show a, ShowRec (ty ki)) => Show (Type ki ty a) where
  showsPrec n (TyVar x) = showsUnaryWith showsPrec "TyVar" n x
  showsPrec n (TyTree x) = showsUnaryWith showsPrecRec "TyTree" n x

instance ShowRec (ty ki) => Show1 (Type ki ty) where
  liftShowsPrec s _ n (TyVar x) = s n x
  liftShowsPrec s sl n (TyTree x) = liftShowsPrec1Rec s sl n x

instance (Bound (ty ki), Bitransversable (ty ki)) => Applicative (Type ki ty) where
  pure = return
  (<*>) = ap

instance (Bound (ty ki), Bitransversable (ty ki)) => Monad (Type ki ty) where
  return = TyVar

  TyVar x >>= f = f x
  TyTree ty >>= f = TyTree (ty >>>= f)

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
