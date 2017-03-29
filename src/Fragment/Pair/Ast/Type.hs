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
module Fragment.Pair.Ast.Type (
    TyFPair
  , AsTyPair(..)
  ) where

import Data.Functor.Classes (showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFPair (ki :: (* -> *) -> * -> *) f a =
  TyPairF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFPair

deriveEq1 ''TyFPair
deriveOrd1 ''TyFPair
deriveShow1 ''TyFPair

instance EqRec (TyFPair ki) where
  liftEqRec eR _ (TyPairF x1 y1) (TyPairF x2 y2) =
    eR x1 x2 && eR y1 y2

instance OrdRec (TyFPair ki) where
  liftCompareRec cR _ (TyPairF x1 y1) (TyPairF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance ShowRec (TyFPair ki) where
  liftShowsPrecRec sR _ _ _ n (TyPairF x y) =
    showsBinaryWith sR sR "TyPairF" n x y

instance Bound (TyFPair ki) where
  TyPairF x y >>>= f = TyPairF (x >>= f) (y >>= f)

instance Bitransversable (TyFPair ki) where
  bitransverse fT fL (TyPairF x y) = TyPairF <$> fT fL x <*> fT fL y

class AsTyPair ki ty where
  _TyPairP :: Prism' (ty ki j a) (TyFPair ki j a)

  _TyPair :: Prism' (Type ki ty a) (Type ki ty a, Type ki ty a)
  _TyPair = _TyTree . _TyPairP . _TyPairF

instance AsTyPair ki TyFPair where
  _TyPairP = id

instance {-# OVERLAPPABLE #-} AsTyPair ki (TySum xs) => AsTyPair ki (TySum (x ': xs)) where
  _TyPairP = _TyNext . _TyPairP

instance {-# OVERLAPPING #-} AsTyPair ki (TySum (TyFPair ': xs)) where
  _TyPairP = _TyNow . _TyPairP
