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
module Fragment.Record.Ast.Term (
    TmFRecord
  , AsTmRecord(..)
  ) where

import Data.Functor.Classes (showsUnaryWith, showsBinaryWith)
import Text.Show (showListWith)

import Bound (Bound(..))
import Control.Lens.Iso (mapping, firsting, seconding)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import qualified Data.Text as T

import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec

data TmFRecord (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmRecordF [(T.Text, f a)]
  | TmRecordIxF (f a) T.Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFRecord

deriveEq1 ''TmFRecord
deriveOrd1 ''TmFRecord
deriveShow1 ''TmFRecord

instance EqRec (TmFRecord ki ty pt) where
  liftEqRec eR _ (TmRecordF xs) (TmRecordF ys) =
    let
      f (l1, x1) (l2, x2) = l1 == l2 && eR x1 x2
    in
      and $ zipWith f xs ys
  liftEqRec eR _ (TmRecordIxF x1 i1) (TmRecordIxF x2 i2) =
    eR x1 x2 && i1 == i2
  liftEqRec _ _ _ _ =
    False

instance OrdRec (TmFRecord ki ty pt) where
  liftCompareRec _ _ (TmRecordF []) (TmRecordF []) = EQ
  liftCompareRec _ _ (TmRecordF []) (TmRecordF (_ : _)) = LT
  liftCompareRec _ _ (TmRecordF (_ : _)) (TmRecordF []) = GT
  liftCompareRec cR c (TmRecordF ((lx, x) : xs)) (TmRecordF ((ly, y) : ys)) =
    case compare lx ly of
      EQ -> case cR x y of
        EQ -> liftCompareRec cR c (TmRecordF xs) (TmRecordF ys)
        z -> z
      z -> z
  liftCompareRec _ _ (TmRecordF _) _ = LT
  liftCompareRec _ _ _ (TmRecordF _) = GT
  liftCompareRec cR _ (TmRecordIxF x1 i1) (TmRecordIxF x2 i2) =
    case cR x1 x2 of
      EQ -> compare i1 i2
      z -> z

instance ShowRec (TmFRecord ki ty pt) where
  liftShowsPrecRec sR _ _ _ n (TmRecordF xs) =
    let
      g m (l, x) = showString ("(" ++ T.unpack l ++ ", ") .
                 sR m x .
                 showString ")"
      f _ ps = showListWith (g 0) ps
    in
      showsUnaryWith f "TmRecordF" n xs
  liftShowsPrecRec sR _ _ _ n (TmRecordIxF x i) =
    showsBinaryWith sR showsPrec "TmRecordIxF" n x i

instance Bound (TmFRecord ki ty pt) where
  TmRecordF tms >>>= f = TmRecordF (fmap (fmap (>>= f)) tms)
  TmRecordIxF tm t >>>= f = TmRecordIxF (tm >>= f) t

instance Bitransversable (TmFRecord ki ty pt) where
  bitransverse fT fL (TmRecordF rs) = TmRecordF <$> traverse (traverse (fT fL)) rs
  bitransverse fT fL (TmRecordIxF r t) = TmRecordIxF <$> fT fL r <*> pure t

class AsTmRecord ki ty pt tm where
  _TmRecordP :: Prism' (tm ki ty pt f a) (TmFRecord ki ty pt f a)

  _TmRecord :: Prism' (Term ki ty pt tm a) [(T.Text, Term ki ty pt tm a)]
  _TmRecord = _Wrapped . _ATerm . _TmRecordP . _TmRecordF . mapping (seconding _Unwrapped)

  _TmRecordIx :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, T.Text)
  _TmRecordIx = _Wrapped . _ATerm . _TmRecordP . _TmRecordIxF . firsting _Unwrapped

instance AsTmRecord ki ty pt TmFRecord where
  _TmRecordP = id

instance {-# OVERLAPPABLE #-} AsTmRecord ki ty pt (TmSum xs) => AsTmRecord ki ty pt (TmSum (x ': xs)) where
  _TmRecordP = _TmNext . _TmRecordP

instance {-# OVERLAPPING #-} AsTmRecord ki ty pt (TmSum (TmFRecord ': xs)) where
  _TmRecordP = _TmNow . _TmRecordP
