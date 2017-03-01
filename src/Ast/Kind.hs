{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Ast.Kind (
    Kind(..)
  , KiSum(..)
  , _KiNow
  , _KiNext
  ) where

import Control.Lens.Prism (Prism', prism)
import Data.Functor.Classes
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

data KiFArr k =
  KiArrF k k
  deriving (Eq, Ord, Show)

newtype Kind k = Kind (k (Kind k))

instance Eq1 k => Eq (Kind k) where
  Kind x == Kind y = liftEq (==) x y

instance Ord1 k => Ord (Kind k) where
  compare (Kind x) (Kind y) = liftCompare compare x y

instance Show1 k => Show (Kind k) where
  showsPrec n (Kind x) = liftShowsPrec showsPrec showList n x

data KiSum (f :: [k -> *]) (a :: k) where
  KiNext :: KiSum f a -> KiSum (g ': f) a
  KiNow :: g a -> KiSum (g ': f) a

_KiNext :: Prism' (KiSum (f ': g) a) (KiSum g a)
_KiNext = prism KiNext $ \x -> case x of
  KiNext y -> Right y
  _ -> Left x

_KiNow :: Prism' (KiSum (f ': g) a) (f a)
_KiNow = prism KiNow $ \x -> case x of
  KiNow y -> Right y
  _ -> Left x

