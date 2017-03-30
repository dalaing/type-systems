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
module Fragment.IsoRec.Ast.Type (
    TyFIsoRec
  , AsTyIsoRec(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFIsoRec (ki :: (* -> *) -> * -> *) f a =
  TyRecF (Scope () f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFIsoRec

instance (Eq1 f, Monad f) => Eq1 (TyFIsoRec ki f) where
  liftEq = $(makeLiftEq ''TyFIsoRec)

instance (Ord1 f, Monad f) => Ord1 (TyFIsoRec ki f) where
  liftCompare = $(makeLiftCompare ''TyFIsoRec)

instance (Show1 f) => Show1 (TyFIsoRec ki f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFIsoRec)

instance EqRec (TyFIsoRec ki) where
  liftEqRec eR e (TyRecF s1) (TyRecF s2) =
    liftEqRec eR e s1 s2

instance OrdRec (TyFIsoRec ki) where
  liftCompareRec cR c (TyRecF s1) (TyRecF s2) =
    liftCompareRec cR c s1 s2

instance ShowRec (TyFIsoRec ki) where
  liftShowsPrecRec sR slR s sl n (TyRecF sc) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TyRecF" n sc

instance Bound (TyFIsoRec ki) where
  TyRecF s >>>= f = TyRecF (s >>>= f)

instance Bitransversable (TyFIsoRec ki) where
  bitransverse fT fL (TyRecF s) = TyRecF <$> bitransverse fT fL s

class (Bound (ty ki), Bitransversable (ty ki)) => AsTyIsoRec ki ty where
  _TyIsoRecP :: Prism' (ty ki j a) (TyFIsoRec ki j a)

  _TyRec :: Prism' (Type ki ty a) (Scope () (TyAst ki ty) (TyAstVar a))
  _TyRec = _Wrapped . _TyAstType . _TyIsoRecP . _TyRecF

instance AsTyIsoRec ki TyFIsoRec where
  _TyIsoRecP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki), Bitransversable (x ki), AsTyIsoRec ki (TySum xs)) => AsTyIsoRec ki (TySum (x ': xs)) where
  _TyIsoRecP = _TyNext . _TyIsoRecP

instance {-# OVERLAPPING #-} (Bound (TySum xs ki), Bitransversable (TySum xs ki)) => AsTyIsoRec ki (TySum (TyFIsoRec ': xs)) where
  _TyIsoRecP = _TyNow . _TyIsoRecP
