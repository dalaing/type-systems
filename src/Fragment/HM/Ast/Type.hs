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
module Fragment.HM.Ast.Type (
    TyFHM
  , AsTyHM(..)
  ) where

import Data.Functor.Classes (showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFHM (ki :: * -> *) f a =
  TyArrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveEq1 ''TyFHM
deriveOrd1 ''TyFHM
deriveShow1 ''TyFHM

makePrisms ''TyFHM

instance EqRec (TyFHM ki) where
  liftEqRec eR _ (TyArrF x1 y1) (TyArrF x2 y2) = eR x1 x2 && eR y1 y2

instance OrdRec (TyFHM ki) where
  liftCompareRec cR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance ShowRec (TyFHM ki) where
  liftShowsPrecRec sR _ _ _ n (TyArrF x y) =
    showsBinaryWith sR sR "TyArrF" n x y

instance Bound (TyFHM ki) where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)

instance Bitransversable (TyFHM ki) where
  bitransverse fT fL (TyArrF x y) = TyArrF <$> fT fL x <*> fT fL y

class AsTyHM ki ty where
  _TyHMP :: Prism' (ty ki j a) (TyFHM ki j a)

  _TyArr :: Prism' (Type ki ty a) (Type ki ty a, Type ki ty a)
  _TyArr = _TyTree . _TyHMP . _TyArrF

instance AsTyHM ki TyFHM where
  _TyHMP = id

instance {-# OVERLAPPABLE #-} AsTyHM ki (TySum xs) => AsTyHM ki (TySum (x ': xs)) where
  _TyHMP = _TyNext . _TyHMP

instance {-# OVERLAPPING #-} AsTyHM ki (TySum (TyFHM ': xs)) where
  _TyHMP = _TyNow . _TyHMP
