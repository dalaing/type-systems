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
module Ast.Kind (
    Kind(..)
  , _KiVar
  , _KiTree
  , KiSum(..)
  , _KiNow
  , _KiNext
  ) where

import Control.Monad (ap)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)
import Data.Traversable (fmapDefault, foldMapDefault)

import Bound (Bound(..))
import Control.Lens.Prism (Prism', prism)
import Control.Lens.TH (makePrisms)

import Data.Bitransversable
import Data.Functor.Rec

data Kind (ki :: (* -> *) -> * -> *) a =
    KiVar a
  | KiTree (ki (Kind ki) a)

makePrisms ''Kind

instance Bitransversable ki => Functor (Kind ki) where
  fmap = fmapDefault

instance Bitransversable ki => Foldable (Kind ki) where
  foldMap = foldMapDefault

instance Bitransversable ki => Traversable (Kind ki) where
  traverse f (KiVar x) = KiVar <$> f x
  traverse f (KiTree x) = KiTree <$> traverseDefault f x

instance (Eq a, EqRec ki) => Eq (Kind ki a) where
  KiVar x == KiVar y = (==) x y
  KiTree x == KiTree y = eqRec x y
  _ == _ = False

instance EqRec ki => Eq1 (Kind ki) where
  liftEq e (KiVar x) (KiVar y) = e x y
  liftEq e (KiTree x) (KiTree y) = liftEq1Rec e x y
  liftEq _ _ _ = False

instance (Ord a, OrdRec ki) => Ord (Kind ki a) where
  compare (KiVar x) (KiVar y) = compare x y
  compare (KiVar _) _ = LT
  compare _ (KiVar _) = GT
  compare (KiTree x) (KiTree y) = compareRec x y

instance OrdRec ki => Ord1 (Kind ki) where
  liftCompare c (KiVar x) (KiVar y) = c x y
  liftCompare _ (KiVar _) _ = LT
  liftCompare _ _ (KiVar _) = GT
  liftCompare c (KiTree x) (KiTree y) = liftCompare1Rec c x y

instance (Show a, ShowRec ki) => Show (Kind ki a) where
  showsPrec n (KiVar x) = showsUnaryWith showsPrec "KiVar" n x
  showsPrec n (KiTree x) = showsUnaryWith showsPrecRec "KiTree" n x

instance ShowRec ki => Show1 (Kind ki) where
  liftShowsPrec s _ n (KiVar x) = s n x
  liftShowsPrec s sl n (KiTree x) = liftShowsPrec1Rec s sl n x

instance (Bound ki, Bitransversable ki) => Applicative (Kind ki) where
  pure = return
  (<*>) = ap

instance (Bound ki, Bitransversable ki) => Monad (Kind ki) where
  return = KiVar

  KiVar x >>= f = f x
  KiTree ty >>= f = KiTree (ty >>>= f)

data KiSum (tys :: [(k1 -> k2 -> *)]) (h :: k1) (a :: k2) where
  KiNext :: KiSum kis h a -> KiSum (ki ': kis) h a
  KiNow :: ki h a -> KiSum (ki ': kis) h a

instance (Eq a, Eq1 h, EqRec (KiSum kis)) => Eq (KiSum kis h a) where
  (==) = eqRec

instance (Eq1 h, EqRec (KiSum kis)) => Eq1 (KiSum kis h) where
  liftEq = liftEq1Rec

instance (Ord a, Ord1 h, OrdRec (KiSum kis)) => Ord (KiSum kis h a) where
  compare = compareRec

instance (Ord1 h, OrdRec (KiSum kis)) => Ord1 (KiSum kis h) where
  liftCompare = liftCompare1Rec

instance (Show a, Show1 h, ShowRec (KiSum kis)) => Show (KiSum kis h a) where
  showsPrec = showsPrecRec

instance (Show1 h, ShowRec (KiSum kis)) => Show1 (KiSum kis h) where
  liftShowsPrec = liftShowsPrec1Rec

instance (Traversable h, Bitransversable (KiSum kis)) => Functor (KiSum kis h) where
  fmap = fmapDefault

instance (Traversable h, Bitransversable (KiSum kis)) => Foldable (KiSum kis h) where
  foldMap = foldMapDefault

instance (Traversable h, Bitransversable (KiSum kis)) => Traversable (KiSum kis h) where
  traverse = traverseDefault

_KiNext :: Prism' (KiSum (ki ': kis) h a) (KiSum kis h a)
_KiNext = prism KiNext $ \x -> case x of
  KiNext y -> Right y
  _ -> Left x

_KiNow :: Prism' (KiSum (ki ': kis) h a) (ki h a)
_KiNow = prism KiNow $ \x -> case x of
  KiNow y -> Right y
  _ -> Left x

instance Bound (KiSum '[]) where
  _ >>>= _ = error "cannot use Bound with an empty list"

instance (Bound x, Bound (KiSum xs)) => Bound (KiSum (x ': xs)) where
  KiNow a >>>= f = KiNow (a >>>= f)
  KiNext n >>>= f = KiNext (n >>>= f)

instance Bitransversable (KiSum '[]) where
  bitransverse _ _ = error "cannot use Bitransversable with an empty list"

instance (Bitransversable x, Bitransversable (KiSum xs)) => Bitransversable (KiSum (x ': xs)) where
  bitransverse fT fL (KiNow a) = KiNow <$> bitransverse fT fL a
  bitransverse fT fL (KiNext n) = KiNext <$> bitransverse fT fL n

instance EqRec (KiSum '[]) where
  liftEqRec _ _ _ _ = True

instance (EqRec x, EqRec (KiSum xs)) => EqRec (KiSum (x ': xs)) where
  liftEqRec eR e (KiNow a1) (KiNow a2) =
    liftEqRec eR e a1 a2
  liftEqRec eR e (KiNext n1) (KiNext n2) =
    liftEqRec eR e n1 n2
  liftEqRec _ _ _ _ =
    False

instance OrdRec (KiSum '[]) where
  liftCompareRec _ _ _ _ = EQ

instance (EqRec x, EqRec (KiSum xs), OrdRec x, OrdRec (KiSum xs)) => OrdRec (KiSum (x ': xs)) where
  liftCompareRec cR c (KiNow a1) (KiNow a2) =
    liftCompareRec cR c a1 a2
  liftCompareRec _ _ (KiNow _) _ =
    LT
  liftCompareRec _ _ _ (KiNow _) =
    GT
  liftCompareRec cR c (KiNext n1) (KiNext n2) =
    liftCompareRec cR c n1 n2

instance ShowRec (KiSum '[]) where
  liftShowsPrecRec _ _ _ _ _ _ = id

instance (ShowRec x, ShowRec (KiSum xs)) => ShowRec (KiSum (x ': xs)) where
  liftShowsPrecRec sR slR s sl m (KiNow a) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "KiSum" m a
  liftShowsPrecRec sR slR s sl m (KiNext n) =
    liftShowsPrecRec sR slR s sl m n
