{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Util.Prisms (
    mkPair
  , mkTriple
  , capFst
  ) where

import Control.Lens

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

capFst :: Iso' ((), b) b
capFst = iso
        (\((), x) -> x)
        (\x -> ((), x))
