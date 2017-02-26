{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Data.Functor.Rec (
    EqRec(..)
  , eqRec
  , liftEq1Rec
  , OrdRec(..)
  , compareRec
  , liftCompare1Rec
  , ShowRec(..)
  , showsPrecRec
  , liftShowsPrec1Rec
  ) where

import Text.Show (showListWith)

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), eq1, compare1, showsPrec1)

import Bound (Scope(..))

class EqRec f where
  liftEqRec :: Eq1 g => (g a -> g b -> Bool) -> (a -> b -> Bool) -> f g a -> f g b -> Bool

eqRec :: (Eq a, Eq1 g, EqRec f) => f g a -> f g a -> Bool
eqRec = liftEqRec eq1 (==)

liftEq1Rec :: (Eq1 g, EqRec f) => (a -> b -> Bool) -> f g a -> f g b -> Bool
liftEq1Rec e = liftEqRec (liftEq e) e

class EqRec f => OrdRec f where
  liftCompareRec :: Ord1 g => (g a -> g b -> Ordering) -> (a -> b -> Ordering) -> f g a -> f g b -> Ordering

compareRec :: (Ord a, Ord1 g, OrdRec f) => f g a -> f g a -> Ordering
compareRec = liftCompareRec compare1 compare

liftCompare1Rec :: (Ord1 g, OrdRec f) => (a -> b -> Ordering) -> f g a -> f g b -> Ordering
liftCompare1Rec c = liftCompareRec (liftCompare c) c

class ShowRec f where
  liftShowsPrecRec :: Show1 g => (Int -> g a -> ShowS) -> ([g a] -> ShowS) -> (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f g a -> ShowS

  liftShowListRec :: Show1 g => (Int -> g a -> ShowS) -> ([g a] -> ShowS) -> (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f g a] -> ShowS
  liftShowListRec sR slR s sl = showListWith (liftShowsPrecRec sR slR s sl 0)

showsPrecRec :: (Show a, Show1 g, ShowRec f) => Int -> f g a -> ShowS
showsPrecRec = liftShowsPrecRec showsPrec1 (liftShowList showsPrec showList) showsPrec showList

liftShowsPrec1Rec :: (Show1 g, ShowRec f) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f g a -> ShowS
liftShowsPrec1Rec s sl n =
  let
    sR = liftShowsPrec s sl
    slR = showListWith (sR 0)
  in
    liftShowsPrecRec sR slR s sl n

-- instances for Bound.Scope

instance Eq b => EqRec (Scope b) where
  liftEqRec eR _ (Scope s1) (Scope s2) = liftEq (liftEq eR) s1 s2

instance Ord b => OrdRec (Scope b) where
  liftCompareRec cR _ (Scope s1) (Scope s2) = liftCompare (liftCompare cR) s1 s2

instance Show b => ShowRec (Scope b) where
  liftShowsPrecRec sR slR _ _ n (Scope s) =
    let
      f m = liftShowsPrec sR slR m
    in
      liftShowsPrec f (showListWith (f n)) n s

