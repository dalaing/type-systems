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
module Fragment.HM.Ast.Term (
    TmFHM
  , AsTmHM(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsBinaryWith, showsUnaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TmFHM (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmLamF (Scope () k a)
  | TmAppF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFHM

instance (Eq1 f, Monad f) => Eq1 (TmFHM ki ty pt f) where
  liftEq = $(makeLiftEq ''TmFHM)

instance (Ord1 f,  Monad f) => Ord1 (TmFHM ki ty pt f) where
  liftCompare = $(makeLiftCompare ''TmFHM)

instance (Show1 f) => Show1 (TmFHM ki ty pt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFHM)

instance EqRec (TmFHM ki ty pt) where
  liftEqRec eR e (TmLamF s1) (TmLamF s2) =
    liftEqRec eR e s1 s2
  liftEqRec eR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFHM ki ty pt) where
  liftCompareRec cR c (TmLamF s1) (TmLamF s2) =
    liftCompareRec cR c s1 s2
  liftCompareRec _ _ (TmLamF _) _ = LT
  liftCompareRec _ _ _ (TmLamF _) = GT
  liftCompareRec cR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFHM ki ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLamF sc) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TmLamF" n sc
  liftShowsPrecRec sR _ _ _ n (TmAppF x y) =
    showsBinaryWith sR sR "TmAppF" n x y

instance Bound (TmFHM ki ty pt) where
  TmLamF s >>>= f = TmLamF (s >>>= f)
  TmAppF x y >>>= f = TmAppF (x >>= f) (y >>= f)

instance Bitransversable (TmFHM ki ty pt) where
  bitransverse fT fL (TmLamF s) = TmLamF <$> bitransverse fT fL s
  bitransverse fT fL (TmAppF x y) = TmAppF <$> fT fL x <*> fT fL y

class (AstBound ki ty pt tm, AstTransversable ki ty pt tm) => AsTmHM ki ty pt tm where
  _TmHMP :: Prism' (tm ki ty pt j a) (TmFHM ki ty pt j a)

  _TmHMLink :: Prism' (Term ki ty pt tm a) (TmFHM ki ty pt (Ast ki ty pt tm) (AstVar a))
  _TmHMLink = _Wrapped . _ATerm . _TmHMP

  _TmLam :: Prism' (Term ki ty pt tm a) (Scope () (Ast ki ty pt tm) (AstVar a))
  _TmLam = _TmHMLink . _TmLamF

  _TmApp :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Term ki ty pt tm a)
  _TmApp = _TmHMLink . _TmAppF . mkPair _Unwrapped _Unwrapped

instance (Bound (ty ki), Bound pt, Bitransversable (ty ki), Bitransversable pt) => AsTmHM ki ty pt TmFHM where
  _TmHMP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki ty pt), Bitransversable (x ki ty pt), AsTmHM ki ty pt (TmSum xs)) => AsTmHM ki ty pt (TmSum (x ': xs)) where
  _TmHMP = _TmNext . _TmHMP

instance {-# OVERLAPPING #-} (Bound (ty ki), Bound pt, Bound (TmSum xs ki ty pt), Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmHM ki ty pt (TmSum (TmFHM ': xs)) where
  _TmHMP = _TmNow . _TmHMP
