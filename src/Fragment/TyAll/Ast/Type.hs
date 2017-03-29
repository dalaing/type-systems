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
module Fragment.TyAll.Ast.Type (
    TyFAll
  , AsTyAll(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsBinaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Prism (Prism', _Just, _Nothing)
import Control.Lens.Iso (iso)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Kind
import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TyFAll (ki :: * -> *) f a =
    TyAllF (Maybe (Kind ki)) (Scope () f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFAll

instance (Eq1 ki, Eq1 ty, Monad ty) => Eq1 (TyFAll ki ty) where
  liftEq = $(makeLiftEq ''TyFAll)

instance (Ord1 ki, Ord1 ty, Monad ty) => Ord1 (TyFAll ki ty) where
  liftCompare = $(makeLiftCompare ''TyFAll)

instance (Show1 ki, Show1 ty) => Show1 (TyFAll ki ty) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFAll)

instance Eq1 ki => EqRec (TyFAll ki) where
  liftEqRec eR e (TyAllF ki1 ty1) (TyAllF ki2 ty2) =
     ki1 == ki2 && liftEqRec eR e ty1 ty2

instance Ord1 ki => OrdRec (TyFAll ki) where
  liftCompareRec cR c (TyAllF ki1 ty1) (TyAllF ki2 ty2) =
    case compare ki1 ki2 of
      EQ -> liftCompareRec cR c ty1 ty2
      z -> z

instance Show1 ki => ShowRec (TyFAll ki) where
  liftShowsPrecRec sR slR s sl n (TyAllF ki ty) =
    showsBinaryWith showsPrec (liftShowsPrecRec sR slR s sl) "TyAllF" n ki ty

instance Bound (TyFAll ki) where
  TyAllF ki ty >>>= f = TyAllF ki (ty >>>= f)

instance Bitransversable (TyFAll ki) where
  bitransverse fT fL (TyAllF ki ty) = TyAllF ki <$> bitransverse fT fL ty

class (Bound (ty ki), Bitransversable (ty ki)) => AsTyAll ki ty where
  _TyAllP :: Prism' (ty ki j a) (TyFAll ki j a)

  _TyAll :: Prism' (Type ki ty a) (Maybe (Kind ki), Scope () (Type ki ty) a)
  _TyAll = _TyTree . _TyAllP . _TyAllF

  _TyAllAnn :: Prism' (Type ki ty a) (Kind ki , Scope () (Type ki ty ) a)
  _TyAllAnn = _TyTree . _TyAllP . _TyAllF . mkPair _Just id

  _TyAllNoAnn :: Prism' (Type ki ty a) (Scope () (Type ki ty) a)
  _TyAllNoAnn =
    let
      capFst = iso
        (\((), x) -> x)
        (\x -> ((), x))
    in
      _TyTree . _TyAllP . _TyAllF . mkPair _Nothing id . capFst

instance AsTyAll ki TyFAll where
  _TyAllP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki), Bitransversable (x ki), AsTyAll ki (TySum xs)) => AsTyAll ki (TySum (x ': xs)) where
  _TyAllP = _TyNext . _TyAllP

instance {-# OVERLAPPING #-} (Bound (TySum xs ki), Bitransversable (TySum xs ki)) => AsTyAll ki (TySum (TyFAll ': xs)) where
  _TyAllP = _TyNow . _TyAllP
