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
module Fragment.STLC.Ast.Term (
    TmFSTLC
  , AsTmSTLC(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsBinaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Type
import Ast.Term
import Util

data TmFSTLC (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmLamF (k a) (Scope () k a)
  | TmAppF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFSTLC

instance (Eq1 k, Monad k) => Eq1 (TmFSTLC ty pt k) where
  liftEq = $(makeLiftEq ''TmFSTLC)

instance (Ord1 k,  Monad k) => Ord1 (TmFSTLC ty pt k) where
  liftCompare = $(makeLiftCompare ''TmFSTLC)

instance (Show1 k) => Show1 (TmFSTLC ty pt k) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFSTLC)

instance EqRec (TmFSTLC ty pt) where
  liftEqRec eR e (TmLamF ty1 s1) (TmLamF ty2 s2) =
    eR ty1 ty2 && liftEqRec eR e s1 s2
  liftEqRec eR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFSTLC ty pt) where
  liftCompareRec cR c (TmLamF ty1 s1) (TmLamF ty2 s2) =
    case cR ty1 ty2 of
      EQ -> liftCompareRec cR c s1 s2
      z -> z
  liftCompareRec _ _ (TmLamF _ _) _ = LT
  liftCompareRec _ _ _ (TmLamF _ _) = GT
  liftCompareRec cR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFSTLC ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLamF ty sc) =
    showsBinaryWith sR (liftShowsPrecRec sR slR s sl) "TmLamF" n ty sc
  liftShowsPrecRec sR _ _ _ n (TmAppF x y) =
    showsBinaryWith sR sR "TmAppF" n x y

instance Bound (TmFSTLC ty pt) where
  TmLamF ty s >>>= f = TmLamF (ty >>= f) (s >>>= f)
  TmAppF x y >>>= f = TmAppF (x >>= f) (y >>= f)

instance Bitransversable (TmFSTLC ty pt) where
  bitransverse fT fL (TmLamF ty s) = TmLamF <$> fT fL ty <*> bitransverse fT fL s
  bitransverse fT fL (TmAppF x y) = TmAppF <$> fT fL x <*> fT fL y

class (AstBound ty pt tm, AstTransversable ty pt tm) => AsTmSTLC ty pt tm where
  _TmSTLCP :: Prism' (tm ty pt k a) (TmFSTLC ty pt k a)

  _TmSTLCLink :: Prism' (Term ty pt tm a) (TmFSTLC ty pt (Ast ty pt tm) (AstVar a))
  _TmSTLCLink = _Wrapped . _ATerm . _TmSTLCP

  _TmLam :: Prism' (Term ty pt tm a) (Type ty a, Scope () (Ast ty pt tm) (AstVar a))
  _TmLam = _TmSTLCLink . _TmLamF . mkPair _Type id

  _TmApp :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmApp = _TmSTLCLink . _TmAppF . mkPair _Unwrapped _Unwrapped

instance (Bound ty, Bound pt, Bitransversable ty, Bitransversable pt) => AsTmSTLC ty pt TmFSTLC where
  _TmSTLCP = id

instance {-# OVERLAPPABLE #-} (Bound (x ty pt), Bitransversable (x ty pt), AsTmSTLC ty pt (TmSum xs)) => AsTmSTLC ty pt (TmSum (x ': xs)) where
  _TmSTLCP = _TmNext . _TmSTLCP

instance {-# OVERLAPPING #-} (Bound ty, Bound pt, Bound (TmSum xs ty pt), Bitransversable ty, Bitransversable pt, Bitransversable (TmSum xs ty pt)) => AsTmSTLC ty pt (TmSum (TmFSTLC ': xs)) where
  _TmSTLCP = _TmNow . _TmSTLCP