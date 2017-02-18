{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module Util (
    Bitransversable(..)
  , traverseDefault
  , EqRec(..)
  , eqRec
  , OrdRec(..)
  , compareRec
  , ShowRec(..)
  , showsPrecRec
  , mkPair
  , mkTriple
  ) where

import Data.Functor.Classes
import Text.Show

import Control.Lens

import Bound.Scope

class Bitransversable s where
  bitransverse :: Applicative f => (forall a b. (a -> f b) -> t a -> f (u b)) -> (c -> f d) -> s t c -> f (s u d)

traverseDefault :: (Applicative f, Traversable r, Bitransversable t) => (a -> f b) -> t r a -> f (t r b)
traverseDefault fL = bitransverse traverse fL

instance Bitransversable (Scope b) where
  bitransverse = bitransverseScope

class EqRec f where
  liftEqRec :: (g a -> g b -> Bool) -> (a -> b -> Bool) -> f g a -> f g b -> Bool

eqRec :: (Eq a, Eq1 g, EqRec f) => f g a -> f g a -> Bool
eqRec = liftEqRec eq1 (==)

class EqRec f => OrdRec f where
  liftCompareRec :: (g a -> g b -> Ordering) -> (a -> b -> Ordering) -> f g a -> f g b -> Ordering

compareRec :: (Ord a, Ord1 g, OrdRec f) => f g a -> f g a -> Ordering
compareRec = liftCompareRec compare1 compare

class ShowRec f where
  liftShowsPrecRec :: (Int -> g a -> ShowS) -> ([g a] -> ShowS) -> (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f g a -> ShowS

  liftShowListRec :: (Int -> g a -> ShowS) -> ([g a] -> ShowS) -> (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f g a] -> ShowS
  liftShowListRec sR slR s sl = showListWith (liftShowsPrecRec sR slR s sl 0)

showsPrecRec :: (Show a, Show1 g, ShowRec f) => Int -> f g a -> ShowS
showsPrecRec = liftShowsPrecRec showsPrec1 (liftShowList showsPrec showList) showsPrec showList

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

