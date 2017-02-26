{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Data.Bitransversable (
    Bitransversable(..)
  , traverseDefault
  ) where

import Bound.Scope (Scope, bitransverseScope)

class Bitransversable s where
  bitransverse :: Applicative f => (forall a b. (a -> f b) -> t a -> f (u b)) -> (c -> f d) -> s t c -> f (s u d)

instance Bitransversable (Scope b) where
  bitransverse = bitransverseScope

traverseDefault :: (Applicative f, Traversable r, Bitransversable t) => (a -> f b) -> t r a -> f (t r b)
traverseDefault = bitransverse traverse
