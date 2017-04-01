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
import Control.Lens.Prism (Prism', _Just, _Nothing, below)
import Control.Lens.Iso (iso)
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Kind
import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TyFAll (ki :: (* -> *) -> * -> *) f a =
    TyAllF (Maybe (f a)) (Scope () f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFAll

instance (EqRec ki, Eq1 ty, Monad ty) => Eq1 (TyFAll ki ty) where
  liftEq = $(makeLiftEq ''TyFAll)

instance (OrdRec ki, Ord1 ty, Monad ty) => Ord1 (TyFAll ki ty) where
  liftCompare = $(makeLiftCompare ''TyFAll)

instance (ShowRec ki, Show1 ty) => Show1 (TyFAll ki ty) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFAll)

instance EqRec ki => EqRec (TyFAll ki) where
  liftEqRec eR e (TyAllF ki1 ty1) (TyAllF ki2 ty2) =
     liftEq eR ki1 ki2 && liftEqRec eR e ty1 ty2

instance OrdRec ki => OrdRec (TyFAll ki) where
  liftCompareRec cR c (TyAllF ki1 ty1) (TyAllF ki2 ty2) =
    case liftCompare cR ki1 ki2 of
      EQ -> liftCompareRec cR c ty1 ty2
      z -> z

instance ShowRec ki => ShowRec (TyFAll ki) where
  liftShowsPrecRec sR slR s sl n (TyAllF ki ty) =
    showsBinaryWith (liftShowsPrec sR slR) (liftShowsPrecRec sR slR s sl) "TyAllF" n ki ty

instance Bound (TyFAll ki) where
  TyAllF Nothing ty >>>= f = TyAllF Nothing (ty >>>= f)
  TyAllF (Just ki) ty >>>= f = TyAllF (Just (ki >>= f)) (ty >>>= f)

instance Bitransversable (TyFAll ki) where
  bitransverse fT fL (TyAllF Nothing ty) = TyAllF <$> pure Nothing <*> bitransverse fT fL ty
  bitransverse fT fL (TyAllF (Just ki) ty) = TyAllF <$> (Just <$> fT fL ki) <*> bitransverse fT fL ty

class (TyAstBound ki ty, TyAstTransversable ki ty) => AsTyAll ki ty where
  _TyAllP :: Prism' (ty ki j a) (TyFAll ki j a)

  _TyAll :: Prism' (Type ki ty a) (Maybe (Kind ki a), Scope () (TyAst ki ty) (TyAstVar a))
  _TyAll = _Wrapped . _TyAstType . _TyAllP . _TyAllF . mkPair (below _TyKind) id

  _TyAllAnn :: Prism' (Type ki ty a) (Kind ki a, Scope () (TyAst ki ty) (TyAstVar a))
  _TyAllAnn = _Wrapped . _TyAstType . _TyAllP . _TyAllF . mkPair (_Just . _TyKind) id

  _TyAllNoAnn :: Prism' (Type ki ty a) (Scope () (TyAst ki ty) (TyAstVar a))
  _TyAllNoAnn = _Wrapped . _TyAstType . _TyAllP . _TyAllF . mkPair _Nothing id . capFst

instance (Bound ki, Bitransversable ki) => AsTyAll ki TyFAll where
  _TyAllP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki), Bitransversable (x ki), AsTyAll ki (TySum xs)) => AsTyAll ki (TySum (x ': xs)) where
  _TyAllP = _TyNext . _TyAllP

instance {-# OVERLAPPING #-} (Bound ki, Bound (TySum xs ki), Bitransversable ki, Bitransversable (TySum xs ki)) => AsTyAll ki (TySum (TyFAll ': xs)) where
  _TyAllP = _TyNow . _TyAllP
