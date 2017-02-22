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
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Type
import Util

data TyFSystemF f a =
    TyArrF (f a) (f a)
  | TyAllF (Scope () f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFSystemF

instance (Eq1 k, Monad k) => Eq1 (TyFSystemF k) where
  liftEq = $(makeLiftEq ''TyFSystemF)

instance (Ord1 k, Monad k) => Ord1 (TyFSystemF k) where
  liftCompare = $(makeLiftCompare ''TyFSystemF)

instance (Show1 k) => Show1 (TyFSystemF k) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFSystemF)

instance EqRec TyFSystemF where
  liftEqRec eR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec eR e (TyAllF s1) (TyAllF s2) =
    liftEqRec eR e s1 s2
  liftEqRec _ _ _ _ = False

instance OrdRec TyFSystemF where
  liftCompareRec cR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x
  liftCompareRec _ _ (TyArrF _ _) _ = LT
  liftCompareRec _ _ _ (TyArrF _ _) = GT
  liftCompareRec cR c (TyAllF s1) (TyAllF s2) =
    liftCompareRec cR c s1 s2

instance ShowRec TyFSystemF where
  liftShowsPrecRec sR _ _ _ n (TyArrF x y) =
    showsBinaryWith sR sR "TyArrF" n x y
  liftShowsPrecRec sR slR s sl n (TyAllF sc) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TyAllF" n sc

instance Bound TyFSystemF where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)
  TyAllF s >>>= f = TyAllF (s >>>= f)

instance Bitransversable TyFSystemF where
  bitransverse fT fL (TyArrF x y) = TyArrF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TyAllF s) = TyAllF <$> bitransverse fT fL s

class (Bound ty, Bitransversable ty) => AsTySystemF ty where
  _TySystemFP :: Prism' (ty k a) (TyFSystemF k a)

  _TyArr :: Prism' (Type ty a) (Type ty a, Type ty a)
  _TyArr = _TyTree . _TySystemFP . _TyArrF

  _TyAll :: Prism' (Type ty a) (Scope () (Type ty) a)
  _TyAll = _TyTree . _TySystemFP . _TyAllF

instance AsTySystemF TyFSystemF where
  _TySystemFP = id

instance {-# OVERLAPPABLE #-} (Bound x, Bitransversable x, AsTySystemF (TySum xs)) => AsTySystemF (TySum (x ': xs)) where
  _TySystemFP = _TyNext . _TySystemFP

instance {-# OVERLAPPING #-} (Bound (TySum xs), Bitransversable (TySum xs)) => AsTySystemF (TySum (TyFSystemF ': xs)) where
  _TySystemFP = _TyNow . _TySystemFP
