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
module Fragment.Variant.Ast.Type (
    TyFVariant
  , AsTyVariant(..)
  ) where

import Data.Functor.Classes (showsUnaryWith)
import Text.Show (showListWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N

import Ast.Type
import Util

data TyFVariant f a =
  TyVariantF (N.NonEmpty (T.Text, f a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFVariant

deriveEq1 ''NonEmpty
deriveOrd1 ''NonEmpty
deriveShow1 ''NonEmpty

deriveEq1 ''TyFVariant
deriveOrd1 ''TyFVariant
deriveShow1 ''TyFVariant

instance EqRec TyFVariant where
  liftEqRec eR _ (TyVariantF vs1) (TyVariantF vs2) =
    let
      f (l1, v1) (l2, v2) = l1 == l2 && eR v1 v2
    in
      and $ N.zipWith f vs1 vs2

instance OrdRec TyFVariant where
  liftCompareRec cR _ (TyVariantF vs1) (TyVariantF vs2) =
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

instance ShowRec TyFVariant where
  liftShowsPrecRec sR _ _ _ n (TyVariantF xs) =
    let
      g m (l, x) = showString ("(" ++ T.unpack l ++ ", ") .
                 sR m x .
                 showString ")"
      f _ ps = showListWith (g 0) ps
    in
      showsUnaryWith f "TyVariantF" n (N.toList xs)

instance Bound TyFVariant where
  TyVariantF tys >>>= f = TyVariantF (fmap (fmap (>>= f)) tys)

instance Bitransversable TyFVariant where
  bitransverse fT fL (TyVariantF ps) = TyVariantF <$> traverse (traverse (fT fL)) ps

class AsTyVariant ty where
  _TyVariantP :: Prism' (ty k a) (TyFVariant k a)

  _TyVariant :: Prism' (Type ty a) (N.NonEmpty (T.Text, Type ty a))
  _TyVariant = _TyTree . _TyVariantP . _TyVariantF

instance AsTyVariant TyFVariant where
  _TyVariantP = id

instance {-# OVERLAPPABLE #-} AsTyVariant (TySum xs) => AsTyVariant (TySum (x ': xs)) where
  _TyVariantP = _TyNext . _TyVariantP

instance {-# OVERLAPPING #-} AsTyVariant (TySum (TyFVariant ': xs)) where
  _TyVariantP = _TyNow . _TyVariantP
