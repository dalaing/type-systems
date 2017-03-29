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
module Fragment.IsoRec.Ast.Term (
    TmFIsoRec
  , AsTmIsoRec(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Type
import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TmFIsoRec (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmFoldF (k a) (k a)
  | TmUnfoldF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFIsoRec

instance (Eq1 f, Monad f) => Eq1 (TmFIsoRec ki ty pt f) where
  liftEq = $(makeLiftEq ''TmFIsoRec)

instance (Ord1 f, Monad f) => Ord1 (TmFIsoRec ki ty pt f) where
  liftCompare = $(makeLiftCompare ''TmFIsoRec)

instance (Show1 f) => Show1 (TmFIsoRec ki ty pt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFIsoRec)

instance EqRec (TmFIsoRec ki ty pt) where
  liftEqRec eR _ (TmFoldF x1 y1) (TmFoldF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec eR _ (TmUnfoldF x1 y1) (TmUnfoldF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFIsoRec ki ty pt) where
  liftCompareRec cR _ (TmFoldF x1 y1) (TmFoldF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z
  liftCompareRec _ _ (TmFoldF _ _) _ = LT
  liftCompareRec _ _ _ (TmFoldF _ _) = GT
  liftCompareRec cR _ (TmUnfoldF x1 y1) (TmUnfoldF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFIsoRec ki ty pt) where
  liftShowsPrecRec sR _ _ _ n (TmFoldF x y) =
    showsBinaryWith sR sR "TmFoldF" n x y
  liftShowsPrecRec sR _ _ _ n (TmUnfoldF x y) =
    showsBinaryWith sR sR "TmUnfoldF" n x y

instance Bound (TmFIsoRec ki ty pt) where
  TmFoldF x y >>>= f = TmFoldF (x >>= f) (y >>= f)
  TmUnfoldF x y >>>= f = TmUnfoldF (x >>= f) (y >>= f)

instance Bitransversable (TmFIsoRec ki ty pt) where
  bitransverse fT fL (TmFoldF x y) = TmFoldF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmUnfoldF x y) = TmUnfoldF <$> fT fL x <*> fT fL y

class (TmAstBound ki ty pt tm, TmAstTransversable ki ty pt tm) => AsTmIsoRec ki ty pt tm where
  _TmIsoRecP :: Prism' (tm ki ty pt f a) (TmFIsoRec ki ty pt f a)

  _TmFold :: Prism' (Term ki ty pt tm a) (Type ki ty a, Term ki ty pt tm a)
  _TmFold = _Wrapped . _TmAstTerm . _TmIsoRecP . _TmFoldF . mkPair _TmType _Unwrapped

  _TmUnfold :: Prism' (Term ki ty pt tm a) (Type ki ty a, Term ki ty pt tm a)
  _TmUnfold = _Wrapped . _TmAstTerm . _TmIsoRecP . _TmUnfoldF . mkPair _TmType _Unwrapped

instance (Bound ki, Bound (ty ki), Bound pt, Bitransversable ki, Bitransversable (ty ki), Bitransversable pt) => AsTmIsoRec ki ty pt TmFIsoRec where
  _TmIsoRecP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki ty pt), Bitransversable (x ki ty pt), AsTmIsoRec ki ty pt (TmSum xs)) => AsTmIsoRec ki ty pt (TmSum (x ': xs)) where
  _TmIsoRecP = _TmNext . _TmIsoRecP

instance {-# OVERLAPPING #-} (Bound ki, Bound (ty ki), Bound pt, Bound (TmSum xs ki ty pt), Bitransversable ki, Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmIsoRec ki ty pt (TmSum (TmFIsoRec ': xs)) where
  _TmIsoRecP = _TmNow . _TmIsoRecP
