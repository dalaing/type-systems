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
module Fragment.SystemF.Ast.Term (
    TmFSystemF
  , AsTmSystemF(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsBinaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Prism (Prism', _Just, _Nothing, below)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Kind
import Ast.Type
import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TmFSystemF (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmLamTyF (Maybe (k a)) (Scope () k a)
  | TmAppTyF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFSystemF

instance (Eq1 f, Monad f) => Eq1 (TmFSystemF ki ty pt f) where
  liftEq = $(makeLiftEq ''TmFSystemF)

instance (Ord1 f, Monad f) => Ord1 (TmFSystemF ki ty pt f) where
  liftCompare = $(makeLiftCompare ''TmFSystemF)

instance (Show1 f) => Show1 (TmFSystemF ki ty pt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFSystemF)

instance EqRec (TmFSystemF ki ty pt) where
  liftEqRec eR e (TmLamTyF k1 s1) (TmLamTyF k2 s2) =
    liftEq eR k1 k2 && liftEqRec eR e s1 s2
  liftEqRec eR _ (TmAppTyF x1 y1) (TmAppTyF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFSystemF ki ty pt) where
  liftCompareRec cR c (TmLamTyF k1 s1) (TmLamTyF k2 s2) =
    case liftCompare cR k1 k2 of
      EQ -> liftCompareRec cR c s1 s2
      z -> z
  liftCompareRec _ _ (TmLamTyF _ _) _ = LT
  liftCompareRec _ _ _ (TmLamTyF _ _) = GT
  liftCompareRec cR _ (TmAppTyF x1 y1) (TmAppTyF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFSystemF ki ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLamTyF k sc) =
    showsBinaryWith (liftShowsPrec sR slR) (liftShowsPrecRec sR slR s sl) "TmLamTyF" n k sc
  liftShowsPrecRec sR _ _ _ n (TmAppTyF x y) =
    showsBinaryWith sR sR "TmAppTyF" n x y

instance Bound (TmFSystemF ki ty pt) where
  TmLamTyF Nothing s >>>= f = TmLamTyF Nothing (s >>>= f)
  TmLamTyF (Just ki) s >>>= f = TmLamTyF (Just (ki >>= f)) (s >>>= f)
  TmAppTyF x y >>>= f = TmAppTyF (x >>= f) (y >>= f)

instance Bitransversable (TmFSystemF ki ty pt) where
  bitransverse fT fL (TmLamTyF Nothing s) = TmLamTyF <$> pure Nothing <*> bitransverse fT fL s
  bitransverse fT fL (TmLamTyF (Just ki) s) = TmLamTyF <$> (Just <$> fT fL ki) <*> bitransverse fT fL s
  bitransverse fT fL (TmAppTyF x y) = TmAppTyF <$> fT fL x <*> fT fL y

class (TmAstBound ki ty pt tm, TmAstTransversable ki ty pt tm) => AsTmSystemF ki ty pt tm where
  _TmSystemFP :: Prism' (tm ki ty pt f a) (TmFSystemF ki ty pt f a)

  _TmLamTy :: Prism' (Term ki ty pt tm a) (Maybe (Kind ki a), Scope () (TmAst ki ty pt tm) (TmAstVar a))
  _TmLamTy = _Wrapped . _TmAstTerm . _TmSystemFP . _TmLamTyF . mkPair (below _TmKind) id

  _TmLamTyAnn :: Prism' (Term ki ty pt tm a) (Kind ki a, Scope () (TmAst ki ty pt tm) (TmAstVar a))
  _TmLamTyAnn = _Wrapped . _TmAstTerm . _TmSystemFP . _TmLamTyF . mkPair (_Just . _TmKind) id

  _TmLamTyNoAnn :: Prism' (Term ki ty pt tm a) (Scope () (TmAst ki ty pt tm) (TmAstVar a))
  _TmLamTyNoAnn = _Wrapped . _TmAstTerm . _TmSystemFP . _TmLamTyF . mkPair _Nothing id . capFst

  _TmAppTy :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Type ki ty a)
  _TmAppTy = _Wrapped . _TmAstTerm . _TmSystemFP . _TmAppTyF . mkPair _Unwrapped _TmType

instance (Bound ki, Bound (ty ki), Bound pt, Bitransversable ki, Bitransversable (ty ki), Bitransversable pt) => AsTmSystemF ki ty pt TmFSystemF where
  _TmSystemFP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki ty pt), Bitransversable (x ki ty pt), AsTmSystemF ki ty pt (TmSum xs)) => AsTmSystemF ki ty pt (TmSum (x ': xs)) where
  _TmSystemFP = _TmNext . _TmSystemFP

instance {-# OVERLAPPING #-} (Bound ki, Bound (ty ki), Bound pt, Bound (TmSum xs ki ty pt), Bitransversable ki, Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmSystemF ki ty pt (TmSum (TmFSystemF ': xs)) where
  _TmSystemFP = _TmNow . _TmSystemFP
