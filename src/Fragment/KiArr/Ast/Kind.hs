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
module Fragment.KiArr.Ast.Kind (
    KiFArr
  , AsKiArr(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.Functor.Classes (showsBinaryWith)

import Ast.Kind
import Data.Bitransversable
import Data.Functor.Rec

data KiFArr f a =
  KiArrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''KiFArr

deriveEq1 ''KiFArr
deriveOrd1 ''KiFArr
deriveShow1 ''KiFArr

instance EqRec KiFArr where
  liftEqRec eR e (KiArrF x1 y1) (KiArrF x2 y2) =
    eR x1 x2 && eR y1 y2

instance OrdRec KiFArr where
  liftCompareRec cR _ (KiArrF x1 y1) (KiArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance ShowRec KiFArr where
  liftShowsPrecRec sR _ _ _ n (KiArrF x y) =
    showsBinaryWith sR sR "KiArrF" n x y

instance Bound KiFArr where
  KiArrF x y >>>= f = KiArrF (x >>= f) (y >>= f)

instance Bitransversable KiFArr where
  bitransverse fT fL (KiArrF x y) = KiArrF <$> fT fL x <*> fT fL y

class AsKiArr ki where
  _KiArrP :: Prism' (ki j a) (KiFArr j a)

  _KiArr :: Prism' (Kind ki a) (Kind ki a, Kind ki a)
  _KiArr = _KiTree . _KiArrP . _KiArrF

instance AsKiArr KiFArr where
  _KiArrP = id

instance {-# OVERLAPPABLE #-} AsKiArr (KiSum xs) => AsKiArr (KiSum (x ': xs)) where
  _KiArrP = _KiNext . _KiArrP

instance {-# OVERLAPPING #-} AsKiArr (KiSum (KiFArr ': xs)) where
  _KiArrP = _KiNow . _KiArrP
