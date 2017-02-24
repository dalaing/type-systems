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
module Fragment.Record.Ast.Pattern (
    PtFRecord
  , AsPtRecord(..)
  ) where

import Data.Functor.Classes (showsUnaryWith)
import Text.Show (showListWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import qualified Data.Text as T

import Ast.Pattern
import Data.Bitransversable
import Data.Functor.Rec

data PtFRecord pt a =
    PtRecordF [(T.Text, pt a)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFRecord

deriveEq1 ''PtFRecord
deriveOrd1 ''PtFRecord
deriveShow1 ''PtFRecord

instance EqRec PtFRecord where
  liftEqRec eR _ (PtRecordF xs) (PtRecordF ys) =
    let
      f (l1, x1) (l2, x2) = l1 == l2 && eR x1 x2
    in
      and $ zipWith f xs ys

instance OrdRec PtFRecord where
  liftCompareRec _ _ (PtRecordF []) (PtRecordF []) = EQ
  liftCompareRec _ _ (PtRecordF []) (PtRecordF (_ : _)) = LT
  liftCompareRec _ _ (PtRecordF (_ : _)) (PtRecordF []) = GT
  liftCompareRec cR c (PtRecordF ((lx, x) : xs)) (PtRecordF ((ly, y) : ys)) =
    case compare lx ly of
      EQ -> case cR x y of
        EQ -> liftCompareRec cR c (PtRecordF xs) (PtRecordF ys)
        z -> z
      z -> z

instance ShowRec PtFRecord where
  liftShowsPrecRec sR _ _ _ n (PtRecordF xs) =
    let
      g m (l, x) = showString ("(" ++ T.unpack l ++ ", ") .
                 sR m x .
                 showString ")"
      f _ ps = showListWith (g 0) ps
    in
      showsUnaryWith f "PtRecordF" n xs

instance Bound PtFRecord where
  PtRecordF pts >>>= f = PtRecordF (fmap (fmap (>>= f)) pts)

instance Bitransversable PtFRecord where
  bitransverse fT fL (PtRecordF rs) = PtRecordF <$> traverse (traverse (fT fL)) rs

class AsPtRecord pt where
  _PtRecordP :: Prism' (pt k a) (PtFRecord k a)

  _PtRecord :: Prism' (Pattern pt a) [(T.Text, Pattern pt a)]
  _PtRecord = _PtTree . _PtRecordP . _PtRecordF

instance AsPtRecord PtFRecord where
  _PtRecordP = id

instance {-# OVERLAPPABLE #-} AsPtRecord (PtSum xs) => AsPtRecord (PtSum (x ': xs)) where
  _PtRecordP = _PtNext . _PtRecordP

instance {-# OVERLAPPING #-} AsPtRecord (PtSum (PtFRecord ': xs)) where
  _PtRecordP = _PtNow . _PtRecordP
