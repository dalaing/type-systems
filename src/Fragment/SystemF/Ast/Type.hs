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
module Fragment.SystemF.Ast.Type (
    TyFSystemF
  , AsTySystemF(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith, showsBinaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Iso (bimapping)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFSystemF (ki :: (* -> *) -> * -> *) f a =
    TyArrF (f a) (f a)
  | TyAllF (Scope () f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFSystemF

instance (Eq1 f, Monad f) => Eq1 (TyFSystemF ki f) where
  liftEq = $(makeLiftEq ''TyFSystemF)

instance (Ord1 f, Monad f) => Ord1 (TyFSystemF ki f) where
  liftCompare = $(makeLiftCompare ''TyFSystemF)

instance (Show1 f) => Show1 (TyFSystemF ki f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFSystemF)

instance EqRec (TyFSystemF ki) where
  liftEqRec eR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec eR e (TyAllF s1) (TyAllF s2) =
    liftEqRec eR e s1 s2
  liftEqRec _ _ _ _ = False

instance OrdRec (TyFSystemF ki) where
  liftCompareRec cR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x
  liftCompareRec _ _ (TyArrF _ _) _ = LT
  liftCompareRec _ _ _ (TyArrF _ _) = GT
  liftCompareRec cR c (TyAllF s1) (TyAllF s2) =
    liftCompareRec cR c s1 s2

instance ShowRec (TyFSystemF ki) where
  liftShowsPrecRec sR _ _ _ n (TyArrF x y) =
    showsBinaryWith sR sR "TyArrF" n x y
  liftShowsPrecRec sR slR s sl n (TyAllF sc) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TyAllF" n sc

instance Bound (TyFSystemF ki) where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)
  TyAllF s >>>= f = TyAllF (s >>>= f)

instance Bitransversable (TyFSystemF ki) where
  bitransverse fT fL (TyArrF x y) = TyArrF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TyAllF s) = TyAllF <$> bitransverse fT fL s

class (Bound (ty ki), Bitransversable (ty ki)) => AsTySystemF ki ty where
  _TySystemFP :: Prism' (ty ki j a) (TyFSystemF ki j a)

  _TyArr :: Prism' (Type ki ty a) (Type ki ty a, Type ki ty a)
  _TyArr = _Wrapped . _TyAstType . _TySystemFP . _TyArrF . bimapping _Unwrapped _Unwrapped

  _TyAll :: Prism' (Type ki ty a) (Scope () (TyAst ki ty) (TyAstVar a))
  _TyAll = _Wrapped . _TyAstType . _TySystemFP . _TyAllF

instance AsTySystemF ki TyFSystemF where
  _TySystemFP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki), Bitransversable (x ki), AsTySystemF ki (TySum xs)) => AsTySystemF ki (TySum (x ': xs)) where
  _TySystemFP = _TyNext . _TySystemFP

instance {-# OVERLAPPING #-} (Bound (TySum xs ki), Bitransversable (TySum xs ki)) => AsTySystemF ki (TySum (TyFSystemF ': xs)) where
  _TySystemFP = _TyNow . _TySystemFP
