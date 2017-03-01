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
module Fragment.Int.Ast.Term (
    TmFInt
  , AsTmInt(..)
  ) where

import Data.Functor.Classes (showsUnaryWith, showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Iso (bimapping)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec

data TmFInt (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmIntF Int
  | TmAddF (f a) (f a)
  | TmMulF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFInt

deriveEq1 ''TmFInt
deriveOrd1 ''TmFInt
deriveShow1 ''TmFInt

instance EqRec (TmFInt ki ty pt) where
  liftEqRec _ _ (TmIntF i) (TmIntF j) = i == j
  liftEqRec eR _ (TmAddF x1 y1) (TmAddF x2 y2) = eR x1 x2 && eR y1 y2
  liftEqRec eR _ (TmMulF x1 y1) (TmMulF x2 y2) = eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFInt ki ty pt) where
  liftCompareRec _ _ (TmIntF i) (TmIntF j) = compare i j
  liftCompareRec _ _ (TmIntF _) _ = LT
  liftCompareRec _ _ _ (TmIntF _) = GT
  liftCompareRec cR _ (TmAddF x1 y1) (TmAddF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z
  liftCompareRec _ _ (TmAddF _ _) _ = LT
  liftCompareRec _ _ _ (TmAddF _ _) = GT
  liftCompareRec cR _ (TmMulF x1 y1) (TmMulF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFInt ki ty pt) where
  liftShowsPrecRec _ _ _ _ n (TmIntF i) =
    showsUnaryWith showsPrec "TmIntF" n i
  liftShowsPrecRec sR _ _ _ n (TmAddF x y) =
    showsBinaryWith sR sR "TmAddF" n x y
  liftShowsPrecRec sR _ _ _ n (TmMulF x y) =
    showsBinaryWith sR sR "TmMulF" n x y

instance Bound (TmFInt ki ty pt) where
  TmIntF b >>>= _ = TmIntF b
  TmAddF x y >>>= f = TmAddF (x >>= f) (y >>= f)
  TmMulF x y >>>= f = TmMulF (x >>= f) (y >>= f)

instance Bitransversable (TmFInt ki ty pt) where
  bitransverse _ _ (TmIntF i) = pure $ TmIntF i
  bitransverse fT fL (TmAddF x y) = TmAddF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmMulF x y) = TmMulF <$> fT fL x <*> fT fL y

class AsTmInt ki ty pt tm where
  _TmIntP :: Prism' (tm ki ty pt f a) (TmFInt ki ty pt f a)

  _TmInt :: Prism' (Term ki ty pt tm a) Int
  _TmInt = _Wrapped . _ATerm . _TmIntP . _TmIntF

  _TmAdd :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Term ki ty pt tm a)
  _TmAdd = _Wrapped . _ATerm . _TmIntP . _TmAddF . bimapping _Unwrapped _Unwrapped

  _TmMul :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Term ki ty pt tm a)
  _TmMul = _Wrapped . _ATerm . _TmIntP . _TmMulF . bimapping _Unwrapped _Unwrapped

instance AsTmInt ki ty pt TmFInt where
  _TmIntP = id

instance {-# OVERLAPPABLE #-} AsTmInt ki ty pt (TmSum xs) => AsTmInt ki ty pt (TmSum (x ': xs)) where
  _TmIntP = _TmNext . _TmIntP

instance {-# OVERLAPPING #-} AsTmInt ki ty pt (TmSum (TmFInt ': xs)) where
  _TmIntP = _TmNow . _TmIntP
