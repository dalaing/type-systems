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
module Ast.Type (
    Type(..)
  , _TyVar
  , _TyTree
  , TySum(..)
  , _TyNow
  , _TyNext
  ) where

import Control.Monad (ap)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)
import Data.Traversable (fmapDefault, foldMapDefault)

import Bound (Bound(..))
import Control.Lens.Prism (Prism', prism)
import Control.Lens.TH (makePrisms)

import Data.Bitransversable
import Data.Functor.Rec

data Type ty a =
    TyVar a
  | TyTree (ty (Type ty) a)

makePrisms ''Type

instance Bitransversable ty => Functor (Type ty) where
  fmap = fmapDefault

instance Bitransversable ty => Foldable (Type ty) where
  foldMap = foldMapDefault

instance Bitransversable ty => Traversable (Type ty) where
  traverse f (TyVar x) = TyVar <$> f x
  traverse f (TyTree x) = TyTree <$> traverseDefault f x

instance (Eq a, EqRec ty) => Eq (Type ty a) where
  TyVar x == TyVar y = (==) x y
  TyTree x == TyTree y = eqRec x y
  _ == _ = False

instance EqRec ty => Eq1 (Type ty) where
  liftEq e (TyVar x) (TyVar y) = e x y
  liftEq e (TyTree x) (TyTree y) = liftEq1Rec e x y
  liftEq _ _ _ = False

instance (Ord a, OrdRec ty) => Ord (Type ty a) where
  compare (TyVar x) (TyVar y) = compare x y
  compare (TyVar _) _ = LT
  compare _ (TyVar _) = GT
  compare (TyTree x) (TyTree y) = compareRec x y

instance OrdRec ty => Ord1 (Type ty) where
  liftCompare c (TyVar x) (TyVar y) = c x y
  liftCompare _ (TyVar _) _ = LT
  liftCompare _ _ (TyVar _) = GT
  liftCompare c (TyTree x) (TyTree y) = liftCompare1Rec c x y

instance (Show a, ShowRec ty) => Show (Type ty a) where
  showsPrec n (TyVar x) = showsUnaryWith showsPrec "TyVar" n x
  showsPrec n (TyTree x) = showsUnaryWith showsPrecRec "TyTree" n x

instance ShowRec ty => Show1 (Type ty) where
  liftShowsPrec s _ n (TyVar x) = s n x
  liftShowsPrec s sl n (TyTree x) = liftShowsPrec1Rec s sl n x

instance (Bound ty, Bitransversable ty) => Applicative (Type ty) where
  pure = return
  (<*>) = ap

instance (Bound ty, Bitransversable ty) => Monad (Type ty) where
  return = TyVar

  TyVar x >>= f = f x
  TyTree ty >>= f = TyTree (ty >>>= f)

data TySum (f :: [(k1 -> k2 -> *)]) (g :: k1) (a :: k2) where
  TyNext :: TySum b g a -> TySum (f ': b) g a
  TyNow :: f g a -> TySum (f ': b) g a

instance (Eq a, Eq1 g, EqRec (TySum xs)) => Eq (TySum xs g a) where
  (==) = eqRec

instance (Eq1 g, EqRec (TySum xs)) => Eq1 (TySum xs g) where
  liftEq = liftEq1Rec

instance (Ord a, Ord1 g, OrdRec (TySum xs)) => Ord (TySum xs g a) where
  compare = compareRec

instance (Ord1 g, OrdRec (TySum xs)) => Ord1 (TySum xs g) where
  liftCompare = liftCompare1Rec

instance (Show a, Show1 g, ShowRec (TySum xs)) => Show (TySum xs g a) where
  showsPrec = showsPrecRec

instance (Show1 g, ShowRec (TySum xs)) => Show1 (TySum xs g) where
  liftShowsPrec = liftShowsPrec1Rec

instance (Traversable g, Bitransversable (TySum xs)) => Functor (TySum xs g) where
  fmap = fmapDefault

instance (Traversable g, Bitransversable (TySum xs)) => Foldable (TySum xs g) where
  foldMap = foldMapDefault

instance (Traversable g, Bitransversable (TySum xs)) => Traversable (TySum xs g) where
  traverse = traverseDefault

_TyNext :: Prism' (TySum (f ': b) g a) (TySum b g a)
_TyNext = prism TyNext $ \x -> case x of
  TyNext y -> Right y
  _ -> Left x

_TyNow :: Prism' (TySum (f ': b) g a) (f g a)
_TyNow = prism TyNow $ \x -> case x of
  TyNow y -> Right y
  _ -> Left x

instance Bound (TySum '[]) where
  _ >>>= _ = error "cannot use Bound with an empty list"

instance (Bound x, Bound (TySum xs)) => Bound (TySum (x ': xs)) where
  TyNow a >>>= f = TyNow (a >>>= f)
  TyNext n >>>= f = TyNext (n >>>= f)

instance Bitransversable (TySum '[]) where
  bitransverse _ _ = error "cannot use Bitransversable with an empty list"

instance (Bitransversable x, Bitransversable (TySum xs)) => Bitransversable (TySum (x ': xs)) where
  bitransverse fT fL (TyNow a) = TyNow <$> bitransverse fT fL a
  bitransverse fT fL (TyNext n) = TyNext <$> bitransverse fT fL n

instance EqRec (TySum '[]) where
  liftEqRec _ _ _ _ = True

instance (EqRec x, EqRec (TySum xs)) => EqRec (TySum (x ': xs)) where
  liftEqRec eR e (TyNow a1) (TyNow a2) =
    liftEqRec eR e a1 a2
  liftEqRec eR e (TyNext n1) (TyNext n2) =
    liftEqRec eR e n1 n2
  liftEqRec _ _ _ _ =
    False

instance OrdRec (TySum '[]) where
  liftCompareRec _ _ _ _ = EQ

instance (EqRec x, EqRec (TySum xs), OrdRec x, OrdRec (TySum xs)) => OrdRec (TySum (x ': xs)) where
  liftCompareRec cR c (TyNow a1) (TyNow a2) =
    liftCompareRec cR c a1 a2
  liftCompareRec _ _ (TyNow _) _ =
    LT
  liftCompareRec _ _ _ (TyNow _) =
    GT
  liftCompareRec cR c (TyNext n1) (TyNext n2) =
    liftCompareRec cR c n1 n2

instance ShowRec (TySum '[]) where
  liftShowsPrecRec _ _ _ _ _ _ = id

instance (ShowRec x, ShowRec (TySum xs)) => ShowRec (TySum (x ': xs)) where
  liftShowsPrecRec sR slR s sl m (TyNow a) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TySum" m a
  liftShowsPrecRec sR slR s sl m (TyNext n) =
    liftShowsPrecRec sR slR s sl m n
