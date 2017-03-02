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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Fragment.SystemFw.Ast.Type (
    TyFSystemFw
  , AsTySystemFw(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsBinaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Kind
import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFSystemFw (ki :: * -> *) f a =
    TyArrF (f a) (f a)
  | TyAllF (Kind ki) (Scope () f a)
  | TyLamF (Kind ki) (Scope () f a)
  | TyAppF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFSystemFw

instance (Eq1 ki, Eq1 f, Monad f) => Eq1 (TyFSystemFw ki f) where
  liftEq = $(makeLiftEq ''TyFSystemFw)

instance (Ord1 ki, Ord1 f, Monad f) => Ord1 (TyFSystemFw ki f) where
  liftCompare = $(makeLiftCompare ''TyFSystemFw)

instance (Show1 ki, Show1 f) => Show1 (TyFSystemFw ki f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFSystemFw)

instance Eq1 ki => EqRec (TyFSystemFw ki) where
  liftEqRec eR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec eR e (TyAllF k1 s1) (TyAllF k2 s2) =
    k1 == k2 && liftEqRec eR e s1 s2
  liftEqRec eR e (TyLamF k1 s1) (TyLamF k2 s2) =
    k1 == k2 && liftEqRec eR e s1 s2
  liftEqRec eR _ (TyAppF x1 y1) (TyAppF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance Ord1 ki => OrdRec (TyFSystemFw ki) where
  liftCompareRec cR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x
  liftCompareRec _ _ (TyArrF _ _) _ = LT
  liftCompareRec _ _ _ (TyArrF _ _) = GT
  liftCompareRec cR c (TyAllF k1 s1) (TyAllF k2 s2) =
    case compare k1 k2 of
      EQ -> liftCompareRec cR c s1 s2
      z -> z
  liftCompareRec _ _ (TyAllF _ _) _ = LT
  liftCompareRec _ _ _ (TyAllF _ _) = GT
  liftCompareRec cR c (TyLamF k1 s1) (TyLamF k2 s2) =
    case compare k1 k2 of
      EQ -> liftCompareRec cR c s1 s2
      z -> z
  liftCompareRec _ _ (TyLamF _ _) _ = LT
  liftCompareRec _ _ _ (TyLamF _ _) = GT
  liftCompareRec cR _ (TyAppF x1 y1) (TyAppF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance Show1 ki => ShowRec (TyFSystemFw ki) where
  liftShowsPrecRec sR _ _ _ n (TyArrF x y) =
    showsBinaryWith sR sR "TyArrF" n x y
  liftShowsPrecRec sR slR s sl n (TyAllF ki sc) =
    showsBinaryWith showsPrec (liftShowsPrecRec sR slR s sl) "TyAllF" n ki sc
  liftShowsPrecRec sR slR s sl n (TyLamF ki sc) =
    showsBinaryWith showsPrec (liftShowsPrecRec sR slR s sl) "TyLamF" n ki sc
  liftShowsPrecRec sR _ _ _ n (TyAppF x y) =
    showsBinaryWith sR sR "TyAppF" n x y

instance Bound (TyFSystemFw ki) where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)
  TyAllF k s >>>= f = TyAllF k (s >>>= f)
  TyLamF k s >>>= f = TyLamF k (s >>>= f)
  TyAppF x y >>>= f = TyAppF (x >>= f) (y >>= f)

instance Bitransversable (TyFSystemFw ki) where
  bitransverse fT fL (TyArrF x y) = TyArrF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TyAllF k s) = TyAllF k <$> bitransverse fT fL s
  bitransverse fT fL (TyLamF k s) = TyLamF k <$> bitransverse fT fL s
  bitransverse fT fL (TyAppF x y) = TyAppF <$> fT fL x <*> fT fL y

class (Bound (ty ki), Bitransversable (ty ki)) => AsTySystemFw ki ty where
  _TySystemFwP :: Prism' (ty ki j a) (TyFSystemFw ki j a)

  _TyArr :: Prism' (Type ki ty a) (Type ki ty a, Type ki ty a)
  _TyArr = _TyTree . _TySystemFwP . _TyArrF

  _TyAll :: Prism' (Type ki ty a) (Kind ki, Scope () (Type ki ty) a)
  _TyAll = _TyTree . _TySystemFwP . _TyAllF

  _TyLam :: Prism' (Type ki ty a) (Kind ki, Scope () (Type ki ty) a)
  _TyLam = _TyTree . _TySystemFwP . _TyLamF

  _TyApp :: Prism' (Type ki ty a) (Type ki ty a, Type ki ty a)
  _TyApp = _TyTree . _TySystemFwP . _TyAppF

instance AsTySystemFw ki TyFSystemFw where
  _TySystemFwP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki), Bitransversable (x ki), AsTySystemFw ki (TySum xs)) => AsTySystemFw ki (TySum (x ': xs)) where
  _TySystemFwP = _TyNext . _TySystemFwP

instance {-# OVERLAPPING #-} (Bound (TySum xs ki), Bitransversable (TySum xs ki)) => AsTySystemFw ki (TySum (TyFSystemFw ': xs)) where
  _TySystemFwP = _TyNow . _TySystemFwP
