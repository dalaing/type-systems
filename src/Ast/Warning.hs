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
module Ast.Warning (
    WarnSum(..)
  , _WarnNow
  , _WarnNext
  ) where

import Control.Lens.Prism (Prism', prism)

data WarnSum (f :: [*]) where
  WarnNext :: WarnSum g -> WarnSum (f ': g)
  WarnNow :: f -> WarnSum (f ': g)

_WarnNext :: Prism' (WarnSum (f ': g)) (WarnSum g)
_WarnNext = prism WarnNext $ \x -> case x of
  WarnNext y -> Right y
  _ -> Left x

_WarnNow :: Prism' (WarnSum (f ': g)) f
_WarnNow = prism WarnNow $ \x -> case x of
  WarnNow y -> Right y
  _ -> Left x

instance Eq (WarnSum '[]) where
  _ == _ = True

instance (Eq x, Eq (WarnSum xs)) => Eq (WarnSum (x ': xs)) where
  WarnNow a1 == WarnNow a2 = a1 == a2
  WarnNext n1 == WarnNext n2 = n1 == n2
  _ == _ = False

instance Ord (WarnSum '[] ) where
  compare _ _ = EQ

instance (Ord x, Ord (WarnSum xs)) => Ord (WarnSum (x ': xs)) where
  compare (WarnNow a1) (WarnNow a2) = compare a1 a2
  compare (WarnNow _) _ = LT
  compare _ (WarnNow _) = GT
  compare (WarnNext n1) (WarnNext n2) = compare n1 n2

instance Show (WarnSum '[]) where
  showsPrec _ _ = id

instance (Show x, Show (WarnSum xs)) => Show (WarnSum (x ': xs)) where
  showsPrec m (WarnNow a) = showString "WarnSum " . showsPrec m a
  showsPrec m (WarnNext n) = showsPrec m n
