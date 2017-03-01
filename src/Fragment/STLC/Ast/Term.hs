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
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TmFSTLC (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmLamF (k a) (Scope () k a)
  | TmAppF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFSTLC

instance (Eq1 f, Monad f) => Eq1 (TmFSTLC ki ty pt f) where
  liftEq = $(makeLiftEq ''TmFSTLC)

instance (Ord1 f,  Monad f) => Ord1 (TmFSTLC ki ty pt f) where
  liftCompare = $(makeLiftCompare ''TmFSTLC)

instance (Show1 f) => Show1 (TmFSTLC ki ty pt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFSTLC)

instance EqRec (TmFSTLC ki ty pt) where
  liftEqRec eR e (TmLamF ty1 s1) (TmLamF ty2 s2) =
    eR ty1 ty2 && liftEqRec eR e s1 s2
  liftEqRec eR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFSTLC ki ty pt) where
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

instance ShowRec (TmFSTLC ki ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLamF ty sc) =
    showsBinaryWith sR (liftShowsPrecRec sR slR s sl) "TmLamF" n ty sc
  liftShowsPrecRec sR _ _ _ n (TmAppF x y) =
    showsBinaryWith sR sR "TmAppF" n x y

instance Bound (TmFSTLC ki ty pt) where
  TmLamF ty s >>>= f = TmLamF (ty >>= f) (s >>>= f)
  TmAppF x y >>>= f = TmAppF (x >>= f) (y >>= f)

instance Bitransversable (TmFSTLC ki ty pt) where
  bitransverse fT fL (TmLamF ty s) = TmLamF <$> fT fL ty <*> bitransverse fT fL s
  bitransverse fT fL (TmAppF x y) = TmAppF <$> fT fL x <*> fT fL y

class (AstBound ki ty pt tm, AstTransversable ki ty pt tm) => AsTmSTLC ki ty pt tm where
  _TmSTLCP :: Prism' (tm ki ty pt f a) (TmFSTLC ki ty pt f a)

  _TmSTLCLink :: Prism' (Term ki ty pt tm a) (TmFSTLC ki ty pt (Ast ki ty pt tm) (AstVar a))
  _TmSTLCLink = _Wrapped . _ATerm . _TmSTLCP

  _TmLam :: Prism' (Term ki ty pt tm a) (Type ki ty a, Scope () (Ast ki ty pt tm) (AstVar a))
  _TmLam = _TmSTLCLink . _TmLamF . mkPair _Type id

  _TmApp :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Term ki ty pt tm a)
  _TmApp = _TmSTLCLink . _TmAppF . mkPair _Unwrapped _Unwrapped

instance (Bound (ty ki), Bound pt, Bitransversable (ty ki), Bitransversable pt) => AsTmSTLC ki ty pt TmFSTLC where
  _TmSTLCP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki ty pt), Bitransversable (x ki ty pt), AsTmSTLC ki ty pt (TmSum xs)) => AsTmSTLC ki ty pt (TmSum (x ': xs)) where
  _TmSTLCP = _TmNext . _TmSTLCP

instance {-# OVERLAPPING #-} (Bound (ty ki), Bound pt, Bound (TmSum xs ki ty pt), Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmSTLC ki ty pt (TmSum (TmFSTLC ': xs)) where
  _TmSTLCP = _TmNow . _TmSTLCP
