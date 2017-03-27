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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Ast.Kind (
    Kind(..)
  , UnifyKind(..)
  , _UnifyKindVar
  , _UnifyKind
  , _UKind
  , KiSum(..)
  , _KiNow
  , _KiNext
  ) where

import Control.Monad (ap)

import Control.Error (note)
import Control.Lens.Prism (Prism', prism)
import Control.Lens.TH (makePrisms, makeWrapped)
import Data.Functor.Classes

newtype Kind k = Kind (k (Kind k))

makeWrapped ''Kind

instance Eq1 k => Eq (Kind k) where
  Kind x == Kind y = liftEq (==) x y

instance Ord1 k => Ord (Kind k) where
  compare (Kind x) (Kind y) = liftCompare compare x y

instance Show1 k => Show (Kind k) where
  showsPrec n (Kind x) = liftShowsPrec showsPrec showList n x

data UnifyKind ki a =
    UnifyKindVar a
  | UnifyKind (ki (UnifyKind ki a))
  deriving (Functor, Foldable, Traversable)

makePrisms ''UnifyKind

instance Functor ki => Applicative (UnifyKind ki) where
  pure = return
  (<*>) = ap

instance Functor ki => Monad (UnifyKind ki) where
  return = UnifyKindVar
  UnifyKindVar x >>= f = f x
  UnifyKind ki >>= f = UnifyKind (fmap (>>= f) ki)

instance (Eq a, Eq1 ki) => Eq (UnifyKind ki a) where
  UnifyKindVar x == UnifyKindVar y = x == y
  UnifyKind x == UnifyKind y = liftEq (==) x y
  _ == _ = False

instance (Ord a, Ord1 ki) => Ord (UnifyKind ki a) where
  compare (UnifyKindVar x) (UnifyKindVar y) = compare x y
  compare (UnifyKindVar _) _ = LT
  compare _ (UnifyKindVar _) = GT
  compare (UnifyKind x) (UnifyKind y) = liftCompare compare x y

kindToUnifyKind :: Functor ki
                => Kind ki
                -> UnifyKind ki a
kindToUnifyKind (Kind ki) =
  UnifyKind (kindToUnifyKind <$> ki)

unifyKindToKind :: Traversable ki
                => UnifyKind ki a
                -> Maybe (Kind ki)
unifyKindToKind (UnifyKindVar _) =
  Nothing
unifyKindToKind (UnifyKind ki) =
  Kind <$> traverse unifyKindToKind ki

_UKind :: Traversable ki => Prism' (UnifyKind ki a) (Kind ki)
_UKind = prism there back
  where
    there = kindToUnifyKind
    back x = note x . unifyKindToKind $ x

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

instance Eq1 (KiSum '[]) where
  liftEq _ _ _ = True

instance (Eq1 x, Eq1 (KiSum xs)) => Eq1 (KiSum (x ': xs)) where
  liftEq e (KiNow x) (KiNow y) = liftEq e x y
  liftEq e (KiNext x) (KiNext y) = liftEq e x y
  liftEq _ _ _ = False

instance Ord1 (KiSum '[]) where
  liftCompare _ _ _ = EQ

instance (Ord1 x, Ord1 (KiSum xs)) => Ord1 (KiSum (x ': xs)) where
  liftCompare c (KiNow x) (KiNow y) = liftCompare c x y
  liftCompare _ (KiNow _) _ = LT
  liftCompare _ _ (KiNow _) = GT
  liftCompare c (KiNext x) (KiNext y) = liftCompare c x y

instance Show1 (KiSum '[]) where
  liftShowsPrec _ _ _ _ _ = ""

instance (Show1 x, Show1 (KiSum xs)) => Show1 (KiSum (x ':xs)) where
  liftShowsPrec s sl n (KiNow x) = liftShowsPrec s sl n x
  liftShowsPrec s sl n (KiNext x) = liftShowsPrec s sl n x




