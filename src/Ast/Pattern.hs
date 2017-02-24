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
module Ast.Pattern (
    Pattern(..)
  , _PtVar
  , _PtTree
  , PtSum(..)
  , _PtNow
  , _PtNext
  ) where

import Control.Monad (ap)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)
import Data.Traversable (fmapDefault, foldMapDefault)

import Control.Lens.Prism (Prism', prism)
import Control.Lens.TH (makePrisms)
import Bound (Bound(..))

import Data.Bitransversable
import Data.Functor.Rec

data Pattern pt a =
    PtVar a
  | PtTree (pt (Pattern pt) a)

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

data PtSum (f :: [(k1 -> k2 -> *)]) (g :: k1) (a :: k2) where
  PtNext :: PtSum b g a -> PtSum (f ': b) g a
  PtNow :: f g a -> PtSum (f ': b) g a

instance (Eq a, Eq1 g, EqRec (PtSum xs)) => Eq (PtSum xs g a) where
  (==) = eqRec

instance (Eq1 g, EqRec (PtSum xs)) => Eq1 (PtSum xs g) where
  liftEq = liftEq1Rec

instance (Ord a, Ord1 g, OrdRec (PtSum xs)) => Ord (PtSum xs g a) where
  compare = compareRec

instance (Ord1 g, OrdRec (PtSum xs)) => Ord1 (PtSum xs g) where
  liftCompare = liftCompare1Rec

instance (Show a, Show1 g, ShowRec (PtSum xs)) => Show (PtSum xs g a) where
  showsPrec = showsPrecRec

instance (Show1 g, ShowRec (PtSum xs)) => Show1 (PtSum xs g) where
  liftShowsPrec = liftShowsPrec1Rec

instance (Traversable g, Bitransversable (PtSum xs)) => Functor (PtSum xs g) where
  fmap = fmapDefault

instance (Traversable g, Bitransversable (PtSum xs)) => Foldable (PtSum xs g) where
  foldMap = foldMapDefault

instance (Traversable g, Bitransversable (PtSum xs)) => Traversable (PtSum xs g) where
  traverse = traverseDefault

_PtNext :: Prism' (PtSum (f ': b) g a) (PtSum b g a)
_PtNext = prism PtNext $ \x -> case x of
  PtNext y -> Right y
  _ -> Left x

_PtNow :: Prism' (PtSum (f ': b) g a) (f g a)
_PtNow = prism PtNow $ \x -> case x of
  PtNow y -> Right y
  _ -> Left x

instance Bound (PtSum '[]) where
  _ >>>= _ = error "cannot use Bound with an empty list"

instance (Bound x, Bound (PtSum xs)) => Bound (PtSum (x ': xs)) where
  PtNow a >>>= f = PtNow (a >>>= f)
  PtNext n >>>= f = PtNext (n >>>= f)

instance Bitransversable (PtSum '[]) where
  bitransverse _ _ = error "cannot use Bitransversable with an empty list"

instance (Bitransversable x, Bitransversable (PtSum xs)) => Bitransversable (PtSum (x ': xs)) where
  bitransverse fT fL (PtNow a) = PtNow <$> bitransverse fT fL a
  bitransverse fT fL (PtNext n) = PtNext <$> bitransverse fT fL n

instance EqRec (PtSum '[]) where
  liftEqRec _ _ _ _ = True

instance (EqRec x, EqRec (PtSum xs)) => EqRec (PtSum (x ': xs)) where
  liftEqRec eR e (PtNow a1) (PtNow a2) =
    liftEqRec eR e a1 a2
  liftEqRec eR e (PtNext n1) (PtNext n2) =
    liftEqRec eR e n1 n2
  liftEqRec _ _ _ _ =
    False

instance OrdRec (PtSum '[]) where
  liftCompareRec _ _ _ _ = EQ

instance (EqRec x, EqRec (PtSum xs), OrdRec x, OrdRec (PtSum xs)) => OrdRec (PtSum (x ': xs)) where
  liftCompareRec cR c (PtNow a1) (PtNow a2) =
    liftCompareRec cR c a1 a2
  liftCompareRec _ _ (PtNow _) _ =
    LT
  liftCompareRec _ _ _ (PtNow _) =
    GT
  liftCompareRec cR c (PtNext n1) (PtNext n2) =
    liftCompareRec cR c n1 n2

instance ShowRec (PtSum '[]) where
  liftShowsPrecRec _ _ _ _ _ _ = id

instance (ShowRec x, ShowRec (PtSum xs)) => ShowRec (PtSum (x ': xs)) where
  liftShowsPrecRec sR slR s sl m (PtNow a) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "PtSum" m a
  liftShowsPrecRec sR slR s sl m (PtNext n) =
    liftShowsPrecRec sR slR s sl m n
