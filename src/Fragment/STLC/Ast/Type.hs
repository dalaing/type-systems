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

data TyFSTLC f a =
  TyArrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveEq1 ''TyFSTLC
deriveOrd1 ''TyFSTLC
deriveShow1 ''TyFSTLC

makePrisms ''TyFSTLC

instance EqRec TyFSTLC where
  liftEqRec eR _ (TyArrF x1 y1) (TyArrF x2 y2) = eR x1 x2 && eR y1 y2

instance OrdRec TyFSTLC where
  liftCompareRec cR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance ShowRec TyFSTLC where
  liftShowsPrecRec sR _ _ _ n (TyArrF x y) =
    showsBinaryWith sR sR "TyArrF" n x y

instance Bound TyFSTLC where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)

instance Bitransversable TyFSTLC where
  bitransverse fT fL (TyArrF x y) = TyArrF <$> fT fL x <*> fT fL y

class AsTySTLC ty where
  _TySTLCP :: Prism' (ty k a) (TyFSTLC k a)

  _TyArr :: Prism' (Type ty a) (Type ty a, Type ty a)
  _TyArr = _TyTree . _TySTLCP . _TyArrF

instance AsTySTLC TyFSTLC where
  _TySTLCP = id

instance {-# OVERLAPPABLE #-} AsTySTLC (TySum xs) => AsTySTLC (TySum (x ': xs)) where
  _TySTLCP = _TyNext . _TySTLCP

instance {-# OVERLAPPING #-} AsTySTLC (TySum (TyFSTLC ': xs)) where
  _TySTLCP = _TyNow . _TySTLCP
