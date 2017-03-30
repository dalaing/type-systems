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
module Fragment.Variant.Ast.Type (
    TyFVariant
  , AsTyVariant(..)
  ) where

import Data.Functor.Classes (showsUnaryWith)
import Text.Show (showListWith)

import Bound (Bound(..))
import Control.Lens.Iso (mapping, seconding)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import qualified Data.Text as T
import qualified Data.List.NonEmpty as N

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec
import Util.NonEmpty

data TyFVariant (ki :: (* -> *) -> * -> *) f a =
  TyVariantF (NE (T.Text, f a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFVariant

deriveEq1 ''TyFVariant
deriveOrd1 ''TyFVariant
deriveShow1 ''TyFVariant

instance EqRec (TyFVariant ki) where
  liftEqRec eR _ (TyVariantF (NE vs1)) (TyVariantF (NE vs2)) =
    let
      f (l1, v1) (l2, v2) = l1 == l2 && eR v1 v2
    in
      and $ N.zipWith f vs1 vs2

instance OrdRec (TyFVariant ki) where
  liftCompareRec cR _ (TyVariantF (NE vs1)) (TyVariantF (NE vs2)) =
    let
      f [] [] = EQ
      f [] (_ : _) = LT
      f (_ : _) [] = GT
      f ((lx, x): xs) ((ly, y): ys) =
        case compare lx ly of
          EQ -> case cR x y of
            EQ -> f xs ys
            z -> z
          z -> z
    in
      f (N.toList vs1) (N.toList vs2)

instance ShowRec (TyFVariant ki) where
  liftShowsPrecRec sR _ _ _ n (TyVariantF (NE xs)) =
    let
      g m (l, x) = showString ("(" ++ T.unpack l ++ ", ") .
                 sR m x .
                 showString ")"
      f _ ps = showListWith (g 0) ps
    in
      showsUnaryWith f "TyVariantF" n (N.toList xs)

instance Bound (TyFVariant ki) where
  TyVariantF tys >>>= f = TyVariantF (fmap (fmap (>>= f)) tys)

instance Bitransversable (TyFVariant ki) where
  bitransverse fT fL (TyVariantF ps) = TyVariantF <$> traverse (traverse (fT fL)) ps

class AsTyVariant ki ty where
  _TyVariantP :: Prism' (ty ki j a) (TyFVariant ki j a)

  _TyVariant :: Prism' (Type ki ty a) (N.NonEmpty (T.Text, Type ki ty a))
  _TyVariant = _Wrapped . _TyAstType . _TyVariantP . _TyVariantF . _Wrapped . mapping (seconding _Unwrapped)

instance AsTyVariant ki TyFVariant where
  _TyVariantP = id

instance {-# OVERLAPPABLE #-} AsTyVariant ki (TySum xs) => AsTyVariant ki (TySum (x ': xs)) where
  _TyVariantP = _TyNext . _TyVariantP

instance {-# OVERLAPPING #-} AsTyVariant ki (TySum (TyFVariant ': xs)) where
  _TyVariantP = _TyNow . _TyVariantP
