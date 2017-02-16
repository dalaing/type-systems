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
  , mkPair
  ) where

import Control.Lens

import Bound.Scope

class Bitransversable s where
  bitransverse :: Applicative f => (forall a b. (a -> f b) -> t a -> f (u b)) -> (c -> f d) -> s t c -> f (s u d)

instance Bitransversable (Scope b) where
  bitransverse = bitransverseScope

mkPair :: Prism' a b -> Prism' c d -> Prism' (a,c) (b, d)
mkPair p1 p2 = prism f g
  where
    f (x, y) = (review p1 x, review p2 y)
    g (x, y) = case (,) <$> preview p1 x <*> preview p2 y of
      Just z -> Right z
      Nothing -> Left (x, y)

