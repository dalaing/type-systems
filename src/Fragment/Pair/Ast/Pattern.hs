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
module Fragment.Pair.Ast.Pattern (
    PtFPair
  , AsPtPair(..)
  ) where

import Data.Functor.Classes (showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Pattern
import Data.Bitransversable
import Data.Functor.Rec

data PtFPair f a =
    PtPairF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFPair

deriveEq1 ''PtFPair
deriveOrd1 ''PtFPair
deriveShow1 ''PtFPair

instance EqRec PtFPair where
  liftEqRec eR _ (PtPairF x1 y1) (PtPairF x2 y2) =
    eR x1 x2 && eR y1 y2

instance OrdRec PtFPair where
  liftCompareRec cR _ (PtPairF x1 y1) (PtPairF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance ShowRec PtFPair where
  liftShowsPrecRec sR _ _ _ n (PtPairF x y) =
    showsBinaryWith sR sR "PtPairF" n x y

instance Bound PtFPair where
  PtPairF x y >>>= f = PtPairF (x >>= f) (y >>= f)

instance Bitransversable PtFPair where
  bitransverse fT fL (PtPairF x y) = PtPairF <$> fT fL x <*> fT fL y

class AsPtPair pt where
  _PtPairP :: Prism' (pt k a) (PtFPair k a)

  _PtPair :: Prism' (Pattern pt a) (Pattern pt a, Pattern pt a)
  _PtPair = _PtTree . _PtPairP . _PtPairF

instance AsPtPair PtFPair where
  _PtPairP = id

instance {-# OVERLAPPABLE #-} AsPtPair (PtSum xs) => AsPtPair (PtSum (x ': xs)) where
  _PtPairP = _PtNext . _PtPairP

instance {-# OVERLAPPING #-} AsPtPair (PtSum (PtFPair ': xs)) where
  _PtPairP = _PtNow . _PtPairP
