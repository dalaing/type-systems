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
module Fragment.Variant.Ast.Pattern (
    PtFVariant
  , AsPtVariant(..)
  ) where

import Data.Functor.Classes (showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import qualified Data.Text as T

import Ast.Pattern
import Util

data PtFVariant f a =
  PtVariantF T.Text (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFVariant

deriveEq1 ''PtFVariant
deriveOrd1 ''PtFVariant
deriveShow1 ''PtFVariant

instance EqRec PtFVariant where
  liftEqRec eR _ (PtVariantF t1 x1) (PtVariantF t2 x2) =
    t1 == t2 && eR x1 x2

instance OrdRec PtFVariant where
  liftCompareRec cR _ (PtVariantF t1 x1) (PtVariantF t2 x2) =
    case compare t1 t2 of
      EQ -> cR x1 x2
      z -> z

instance ShowRec PtFVariant where
  liftShowsPrecRec sR _ _ _ n (PtVariantF t x) =
    showsBinaryWith showsPrec sR "PtVariantF" n t x

instance Bound PtFVariant where
  PtVariantF l p >>>= f = PtVariantF l (p >>= f)

instance Bitransversable PtFVariant where
  bitransverse fT fL (PtVariantF l pt) = PtVariantF l <$> fT fL pt

class AsPtVariant pt where
  _PtVariantP :: Prism' (pt k a) (PtFVariant k a)

  _PtVariant :: Prism' (Pattern pt a) (T.Text, Pattern pt a)
  _PtVariant = _PtTree . _PtVariantP . _PtVariantF

instance AsPtVariant PtFVariant where
  _PtVariantP = id

instance {-# OVERLAPPABLE #-} AsPtVariant (PtSum xs) => AsPtVariant (PtSum (x ': xs)) where
  _PtVariantP = _PtNext . _PtVariantP

instance {-# OVERLAPPING #-} AsPtVariant (PtSum (PtFVariant ': xs)) where
  _PtVariantP = _PtNow . _PtVariantP
