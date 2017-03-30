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
module Fragment.TyArr.Ast.Type (
    TyFArr
  , AsTyArr(..)
  ) where

import Data.Functor.Classes (showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Iso (bimapping)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFArr (ki :: (* -> *) -> * -> *) f a =
  TyArrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveEq1 ''TyFArr
deriveOrd1 ''TyFArr
deriveShow1 ''TyFArr

makePrisms ''TyFArr

instance EqRec (TyFArr ki) where
  liftEqRec eR _ (TyArrF x1 y1) (TyArrF x2 y2) = eR x1 x2 && eR y1 y2

instance OrdRec (TyFArr ki) where
  liftCompareRec cR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance ShowRec (TyFArr ki) where
  liftShowsPrecRec sR _ _ _ n (TyArrF x y) =
    showsBinaryWith sR sR "TyArrF" n x y

instance Bound (TyFArr ki) where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)

instance Bitransversable (TyFArr ki) where
  bitransverse fT fL (TyArrF x y) = TyArrF <$> fT fL x <*> fT fL y

class AsTyArr ki ty where
  _TyArrP :: Prism' (ty ki j a) (TyFArr ki j a)

  _TyArr :: Prism' (Type ki ty a) (Type ki ty a, Type ki ty a)
  _TyArr = _Wrapped . _TyAstType . _TyArrP . _TyArrF . bimapping _Unwrapped _Unwrapped

instance AsTyArr ki TyFArr where
  _TyArrP = id

instance {-# OVERLAPPABLE #-} AsTyArr ki (TySum xs) => AsTyArr ki (TySum (x ': xs)) where
  _TyArrP = _TyNext . _TyArrP

instance {-# OVERLAPPING #-} AsTyArr ki (TySum (TyFArr ': xs)) where
  _TyArrP = _TyNow . _TyArrP
