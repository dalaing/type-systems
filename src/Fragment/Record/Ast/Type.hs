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
module Fragment.Record.Ast.Type (
    TyFRecord
  , AsTyRecord(..)
  ) where

import Data.Functor.Classes (showsUnaryWith)
import Text.Show (showListWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import qualified Data.Text as T

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFRecord (ki :: * -> *) f a =
  TyRecordF [(T.Text, f a)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFRecord

deriveEq1 ''TyFRecord
deriveOrd1 ''TyFRecord
deriveShow1 ''TyFRecord

instance EqRec (TyFRecord ki) where
  liftEqRec eR _ (TyRecordF xs) (TyRecordF ys) =
    let
      f (l1, x1) (l2, x2) = l1 == l2 && eR x1 x2
    in
      and $ zipWith f xs ys

instance OrdRec (TyFRecord ki) where
  liftCompareRec _ _ (TyRecordF []) (TyRecordF []) = EQ
  liftCompareRec _ _ (TyRecordF []) (TyRecordF (_ : _)) = LT
  liftCompareRec _ _ (TyRecordF (_ : _)) (TyRecordF []) = GT
  liftCompareRec cR c (TyRecordF ((lx, x) : xs)) (TyRecordF ((ly, y) : ys)) =
    case compare lx ly of
      EQ -> case cR x y of
        EQ -> liftCompareRec cR c (TyRecordF xs) (TyRecordF ys)
        z -> z
      z -> z

instance ShowRec (TyFRecord ki) where
  liftShowsPrecRec sR _ _ _ n (TyRecordF xs) =
    let
      g m (l, x) = showString ("(" ++ T.unpack l ++ ", ") .
                 sR m x .
                 showString ")"
      f _ ps = showListWith (g 0) ps
    in
      showsUnaryWith f "TyRecordF" n xs

instance Bound (TyFRecord ki) where
  TyRecordF tys >>>= f = TyRecordF (fmap (fmap (>>= f)) tys)

instance Bitransversable (TyFRecord ki) where
  bitransverse fT fL (TyRecordF rs) = TyRecordF <$> traverse (traverse (fT fL)) rs

class AsTyRecord ki ty where
  _TyRecordP :: Prism' (ty ki j a) (TyFRecord ki j a)

  _TyRecord :: Prism' (Type ki ty a) [(T.Text, Type ki ty a)]
  _TyRecord = _TyTree . _TyRecordP . _TyRecordF

instance AsTyRecord ki TyFRecord where
  _TyRecordP = id

instance {-# OVERLAPPABLE #-} AsTyRecord ki (TySum xs) => AsTyRecord ki (TySum (x ': xs)) where
  _TyRecordP = _TyNext . _TyRecordP

instance {-# OVERLAPPING #-} AsTyRecord ki (TySum (TyFRecord ': xs)) where
  _TyRecordP = _TyNow . _TyRecordP
