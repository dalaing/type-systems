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
module Fragment.Pair.Ast.Term (
    TmFPair
  , AsTmPair(..)
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

data TmFPair (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmPairF (f a) (f a)
  | TmFstF (f a)
  | TmSndF (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFPair

deriveEq1 ''TmFPair
deriveOrd1 ''TmFPair
deriveShow1 ''TmFPair

instance EqRec (TmFPair ty pt) where
  liftEqRec eR _ (TmPairF x1 y1) (TmPairF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec eR _ (TmFstF x1) (TmFstF x2) =
    eR x1 x2
  liftEqRec eR _ (TmSndF x1) (TmSndF x2) =
    eR x1 x2
  liftEqRec _ _ _ _ =
    False

instance OrdRec (TmFPair ty pt) where
  liftCompareRec cR _ (TmPairF x1 y1) (TmPairF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x
  liftCompareRec _ _ (TmPairF _ _) _ = LT
  liftCompareRec _ _ _ (TmPairF _ _) = GT
  liftCompareRec cR _ (TmFstF x1) (TmFstF x2) =
    cR x1 x2
  liftCompareRec _ _ (TmFstF _) _ = LT
  liftCompareRec _ _ _ (TmFstF _) = GT
  liftCompareRec cR _ (TmSndF x1) (TmSndF x2) =
    cR x1 x2

instance ShowRec (TmFPair ty pt) where
  liftShowsPrecRec sR _ _ _ n (TmPairF x y) =
    showsBinaryWith sR sR "TmPairF" n x y
  liftShowsPrecRec sR _ _ _ n (TmFstF x) =
    showsUnaryWith sR "TmFstF" n x
  liftShowsPrecRec sR _ _ _ n (TmSndF x) =
    showsUnaryWith sR "TmSndF" n x

instance Bound (TmFPair ty pt) where
  TmPairF x y >>>= f = TmPairF (x >>= f) (y >>= f)
  TmFstF x >>>= f = TmFstF (x >>= f)
  TmSndF x >>>= f = TmSndF (x >>= f)

instance Bitransversable (TmFPair ty pt) where
  bitransverse fT fL (TmPairF x y) = TmPairF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmFstF x) = TmFstF <$> fT fL x
  bitransverse fT fL (TmSndF x) = TmSndF <$> fT fL x

class AsTmPair ty pt tm where
  _TmPairP :: Prism' (tm ty pt k a) (TmFPair ty pt k a)

  _TmPair :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmPair = _Wrapped . _ATerm . _TmPairP . _TmPairF . bimapping _Unwrapped _Unwrapped

  _TmFst :: Prism' (Term ty pt tm a) (Term ty pt tm a)
  _TmFst = _Wrapped . _ATerm . _TmPairP . _TmFstF . _Unwrapped

  _TmSnd :: Prism' (Term ty pt tm a) (Term ty pt tm a)
  _TmSnd = _Wrapped . _ATerm . _TmPairP . _TmSndF . _Unwrapped

instance AsTmPair ty pt TmFPair where
  _TmPairP = id

instance {-# OVERLAPPABLE #-} AsTmPair ty pt (TmSum xs) => AsTmPair ty pt (TmSum (x ': xs)) where
  _TmPairP = _TmNext . _TmPairP

instance {-# OVERLAPPING #-} AsTmPair ty pt (TmSum (TmFPair ': xs)) where
  _TmPairP = _TmNow . _TmPairP
