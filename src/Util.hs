{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Util (
    TSum(..)
  , _TNext
  , _TAdd
  , Bitransversable(..)
  , traverseDefault
  , EqRec(..)
  , eqRec
  , liftEq1Rec
  , OrdRec(..)
  , compareRec
  , liftCompare1Rec
  , ShowRec(..)
  , showsPrecRec
  , liftShowsPrec1Rec
  , mkPair
  , mkTriple
  ) where

import Data.Traversable

import Data.Functor.Classes
import Text.Show

import Control.Lens

import Bound
import Bound.Scope

data TSum (f :: [(k1 -> k2 -> *)]) (g :: k1) (a :: k2) where
  TNext :: TSum b g a -> TSum (f ': b) g a
  TAdd :: f g a -> TSum (f ': b) g a

instance (Eq a, Eq1 g, EqRec (TSum xs)) => Eq (TSum xs g a) where
  (==) = eqRec

instance (Eq1 g, EqRec (TSum xs)) => Eq1 (TSum xs g) where
  liftEq = liftEq1Rec

instance (Ord a, Ord1 g, OrdRec (TSum xs)) => Ord (TSum xs g a) where
  compare = compareRec

instance (Ord1 g, OrdRec (TSum xs)) => Ord1 (TSum xs g) where
  liftCompare = liftCompare1Rec

instance (Show a, Show1 g, ShowRec (TSum xs)) => Show (TSum xs g a) where
  showsPrec = showsPrecRec

instance (Show1 g, ShowRec (TSum xs)) => Show1 (TSum xs g) where
  liftShowsPrec = liftShowsPrec1Rec

instance (Traversable g, Bitransversable (TSum xs)) => Functor (TSum xs g) where
  fmap = fmapDefault

instance (Traversable g, Bitransversable (TSum xs)) => Foldable (TSum xs g) where
  foldMap = foldMapDefault

instance (Traversable g, Bitransversable (TSum xs)) => Traversable (TSum xs g) where
  traverse = traverseDefault

_TNext :: Prism' (TSum (f ': b) g a) (TSum b g a)
_TNext = prism TNext $ \x -> case x of
  TNext y -> Right y
  _ -> Left x

_TAdd :: Prism' (TSum (f ': b) g a) (f g a)
_TAdd = prism TAdd $ \x -> case x of
  TAdd y -> Right y
  _ -> Left x

instance Bound (TSum '[]) where
  _ >>>= _ = error "cannot use Bound with an empty list"

instance (Bound x, Bound (TSum xs)) => Bound (TSum (x ': xs)) where
  TAdd a >>>= f = TAdd (a >>>= f)
  TNext n >>>= f = TNext (n >>>= f)

class Bitransversable s where
  bitransverse :: Applicative f => (forall a b. (a -> f b) -> t a -> f (u b)) -> (c -> f d) -> s t c -> f (s u d)

instance Bitransversable (Scope b) where
  bitransverse = bitransverseScope

instance Bitransversable (TSum '[]) where
  bitransverse _ _ = error "cannot use Bitransversable with an empty list"

instance (Bitransversable x, Bitransversable (TSum xs)) => Bitransversable (TSum (x ': xs)) where
  bitransverse fT fL (TAdd a) = TAdd <$> bitransverse fT fL a
  bitransverse fT fL (TNext n) = TNext <$> bitransverse fT fL n

traverseDefault :: (Applicative f, Traversable r, Bitransversable t) => (a -> f b) -> t r a -> f (t r b)
traverseDefault = bitransverse traverse

class EqRec f where
  liftEqRec :: Eq1 g => (g a -> g b -> Bool) -> (a -> b -> Bool) -> f g a -> f g b -> Bool

instance Eq b => EqRec (Scope b) where
  liftEqRec eR _ (Scope s1) (Scope s2) = liftEq (liftEq eR) s1 s2

instance EqRec (TSum '[]) where
  liftEqRec _ _ _ _ = True

instance (EqRec x, EqRec (TSum xs)) => EqRec (TSum (x ': xs)) where
  liftEqRec eR e (TAdd a1) (TAdd a2) =
    liftEqRec eR e a1 a2
  liftEqRec eR e (TNext n1) (TNext n2) =
    liftEqRec eR e n1 n2
  liftEqRec _ _ _ _ =
    False

eqRec :: (Eq a, Eq1 g, EqRec f) => f g a -> f g a -> Bool
eqRec = liftEqRec eq1 (==)

liftEq1Rec :: (Eq1 g, EqRec f) => (a -> b -> Bool) -> f g a -> f g b -> Bool
liftEq1Rec e = liftEqRec (liftEq e) e

class EqRec f => OrdRec f where
  liftCompareRec :: Ord1 g => (g a -> g b -> Ordering) -> (a -> b -> Ordering) -> f g a -> f g b -> Ordering

instance Ord b => OrdRec (Scope b) where
  liftCompareRec cR _ (Scope s1) (Scope s2) = liftCompare (liftCompare cR) s1 s2

instance OrdRec (TSum '[]) where
  liftCompareRec _ _ _ _ = EQ

instance (EqRec x, EqRec (TSum xs), OrdRec x, OrdRec (TSum xs)) => OrdRec (TSum (x ': xs)) where
  liftCompareRec cR c (TAdd a1) (TAdd a2) =
    liftCompareRec cR c a1 a2
  liftCompareRec _ _ (TAdd _) _ =
    LT
  liftCompareRec _ _ _ (TAdd _) =
    GT
  liftCompareRec cR c (TNext n1) (TNext n2) =
    liftCompareRec cR c n1 n2

compareRec :: (Ord a, Ord1 g, OrdRec f) => f g a -> f g a -> Ordering
compareRec = liftCompareRec compare1 compare

liftCompare1Rec :: (Ord1 g, OrdRec f) => (a -> b -> Ordering) -> f g a -> f g b -> Ordering
liftCompare1Rec c = liftCompareRec (liftCompare c) c

class ShowRec f where
  liftShowsPrecRec :: Show1 g => (Int -> g a -> ShowS) -> ([g a] -> ShowS) -> (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f g a -> ShowS

  liftShowListRec :: Show1 g => (Int -> g a -> ShowS) -> ([g a] -> ShowS) -> (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f g a] -> ShowS
  liftShowListRec sR slR s sl = showListWith (liftShowsPrecRec sR slR s sl 0)

instance Show b => ShowRec (Scope b) where
  liftShowsPrecRec sR slR _ _ n (Scope s) =
    let
      f m = liftShowsPrec sR slR m
    in
      liftShowsPrec f (showListWith (f n)) n s

instance ShowRec (TSum '[]) where
  liftShowsPrecRec _ _ _ _ _ _ = id

instance (ShowRec x, ShowRec (TSum xs)) => ShowRec (TSum (x ': xs)) where
  liftShowsPrecRec sR slR s sl m (TAdd a) =
    liftShowsPrecRec sR slR s sl m a
    -- TODO this might be a better way of showing what is going on
    -- showsUnaryWith (liftShowsPrecRec sR slR s sl) "TSum" m a
  liftShowsPrecRec sR slR s sl m (TNext n) =
    liftShowsPrecRec sR slR s sl m n

showsPrecRec :: (Show a, Show1 g, ShowRec f) => Int -> f g a -> ShowS
showsPrecRec = liftShowsPrecRec showsPrec1 (liftShowList showsPrec showList) showsPrec showList

liftShowsPrec1Rec :: (Show1 g, ShowRec f) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f g a -> ShowS
liftShowsPrec1Rec s sl n =
  let
    sR = liftShowsPrec s sl
    slR = showListWith (sR 0)
  in
    liftShowsPrecRec sR slR s sl n

mkPair :: Prism' a b -> Prism' c d -> Prism' (a,c) (b, d)
mkPair p1 p2 = prism f g
  where
    f (x, y) = (review p1 x, review p2 y)
    g (x, y) = case (,) <$> preview p1 x <*> preview p2 y of
      Just z -> Right z
      Nothing -> Left (x, y)

mkTriple :: Prism' a b -> Prism' c d -> Prism' e f -> Prism' (a, c, e) (b, d, f)
mkTriple p1 p2 p3 = prism f g
  where
    f (x, y, z) = (review p1 x, review p2 y, review p3 z)
    g (x, y, z) = case (\a b c -> (a, b, c)) <$> preview p1 x <*> preview p2 y <*> preview p3 z of 
      Just a -> Right a
      Nothing -> Left (x, y, z)
