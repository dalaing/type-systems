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
import Control.Lens.Prism (Prism', below, _Just, _Nothing)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Kind
import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TyFSystemFw (ki :: (* -> *) -> * -> *) f a =
    TyLamF (Maybe (f a)) (Scope () f a)
  | TyAppF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFSystemFw

instance (EqRec ki, Eq1 f, Monad f) => Eq1 (TyFSystemFw ki f) where
  liftEq = $(makeLiftEq ''TyFSystemFw)

instance (OrdRec ki, Ord1 f, Monad f) => Ord1 (TyFSystemFw ki f) where
  liftCompare = $(makeLiftCompare ''TyFSystemFw)

instance (ShowRec ki, Show1 f) => Show1 (TyFSystemFw ki f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFSystemFw)

instance EqRec ki => EqRec (TyFSystemFw ki) where
  liftEqRec eR e (TyLamF k1 s1) (TyLamF k2 s2) =
    liftEq eR k1 k2 && liftEqRec eR e s1 s2
  liftEqRec eR _ (TyAppF x1 y1) (TyAppF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec ki => OrdRec (TyFSystemFw ki) where
  liftCompareRec cR c (TyLamF k1 s1) (TyLamF k2 s2) =
    case liftCompare cR k1 k2 of
      EQ -> liftCompareRec cR c s1 s2
      z -> z
  liftCompareRec _ _ (TyLamF _ _) _ = LT
  liftCompareRec _ _ _ (TyLamF _ _) = GT
  liftCompareRec cR _ (TyAppF x1 y1) (TyAppF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance ShowRec ki => ShowRec (TyFSystemFw ki) where
  liftShowsPrecRec sR slR s sl n (TyLamF ki sc) =
    showsBinaryWith (liftShowsPrec sR slR) (liftShowsPrecRec sR slR s sl) "TyLamF" n ki sc
  liftShowsPrecRec sR _ _ _ n (TyAppF x y) =
    showsBinaryWith sR sR "TyAppF" n x y

instance Bound (TyFSystemFw ki) where
  TyLamF Nothing s >>>= f = TyLamF Nothing (s >>>= f)
  TyLamF (Just ki) s >>>= f = TyLamF (Just (ki >>= f)) (s >>>= f)
  TyAppF x y >>>= f = TyAppF (x >>= f) (y >>= f)

instance Bitransversable (TyFSystemFw ki) where
  bitransverse fT fL (TyLamF Nothing s) = TyLamF <$> pure Nothing <*> bitransverse fT fL s
  bitransverse fT fL (TyLamF (Just ki) s) = TyLamF <$> (Just <$> fT fL ki) <*> bitransverse fT fL s
  bitransverse fT fL (TyAppF x y) = TyAppF <$> fT fL x <*> fT fL y

class (TyAstBound ki ty, TyAstTransversable ki ty) => AsTySystemFw ki ty where
  _TySystemFwP :: Prism' (ty ki j a) (TyFSystemFw ki j a)

  _TyLam :: Prism' (Type ki ty a) (Maybe (Kind ki a), Scope () (TyAst ki ty) (TyAstVar a))
  _TyLam = _Wrapped . _TyAstType . _TySystemFwP . _TyLamF . mkPair (below _TyKind) id

  _TyLamAnn :: Prism' (Type ki ty a) (Kind ki a, Scope () (TyAst ki ty) (TyAstVar a))
  _TyLamAnn = _Wrapped . _TyAstType . _TySystemFwP . _TyLamF . mkPair (_Just . _TyKind) id

  _TyLamNoAnn :: Prism' (Type ki ty a) (Scope () (TyAst ki ty) (TyAstVar a))
  _TyLamNoAnn = _Wrapped . _TyAstType . _TySystemFwP . _TyLamF . mkPair _Nothing id . capFst

  _TyApp :: Prism' (Type ki ty a) (Type ki ty a, Type ki ty a)
  _TyApp = _Wrapped . _TyAstType . _TySystemFwP . _TyAppF . mkPair _Unwrapped _Unwrapped

instance (Bound ki, Bitransversable ki) => AsTySystemFw ki TyFSystemFw where
  _TySystemFwP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki), Bitransversable (x ki), AsTySystemFw ki (TySum xs)) => AsTySystemFw ki (TySum (x ': xs)) where
  _TySystemFwP = _TyNext . _TySystemFwP

instance {-# OVERLAPPING #-} (Bound ki, Bound (TySum xs ki), Bitransversable ki, Bitransversable (TySum xs ki)) => AsTySystemFw ki (TySum (TyFSystemFw ': xs)) where
  _TySystemFwP = _TyNow . _TySystemFwP
