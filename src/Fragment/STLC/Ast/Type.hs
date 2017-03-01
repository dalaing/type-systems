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
module Fragment.STLC.Ast.Type (
    TyFSTLC
  , AsTySTLC(..)
  ) where

import Data.Functor.Classes (showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFSTLC (ki :: * -> *) f a =
  TyArrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveEq1 ''TyFSTLC
deriveOrd1 ''TyFSTLC
deriveShow1 ''TyFSTLC

makePrisms ''TyFSTLC

instance EqRec (TyFSTLC ki) where
  liftEqRec eR _ (TyArrF x1 y1) (TyArrF x2 y2) = eR x1 x2 && eR y1 y2

instance OrdRec (TyFSTLC ki) where
  liftCompareRec cR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance ShowRec (TyFSTLC ki) where
  liftShowsPrecRec sR _ _ _ n (TyArrF x y) =
    showsBinaryWith sR sR "TyArrF" n x y

instance Bound (TyFSTLC ki) where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)

instance Bitransversable (TyFSTLC ki) where
  bitransverse fT fL (TyArrF x y) = TyArrF <$> fT fL x <*> fT fL y

class AsTySTLC ki ty where
  _TySTLCP :: Prism' (ty ki j a) (TyFSTLC ki j a)

  _TyArr :: Prism' (Type ki ty a) (Type ki ty a, Type ki ty a)
  _TyArr = _TyTree . _TySTLCP . _TyArrF

instance AsTySTLC ki TyFSTLC where
  _TySTLCP = id

instance {-# OVERLAPPABLE #-} AsTySTLC ki (TySum xs) => AsTySTLC ki (TySum (x ': xs)) where
  _TySTLCP = _TyNext . _TySTLCP

instance {-# OVERLAPPING #-} AsTySTLC ki (TySum (TyFSTLC ': xs)) where
  _TySTLCP = _TyNow . _TySTLCP
