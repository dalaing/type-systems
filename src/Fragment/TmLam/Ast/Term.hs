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
module Fragment.TmLam.Ast.Term (
    TmFLam
  , AsTmLam(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsBinaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Iso (iso)
import Control.Lens.Prism (Prism', below, _Just, _Nothing)
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Type
import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TmFLam (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmLamF (Maybe (k a)) (Scope () k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFLam

instance (Eq1 f, Monad f) => Eq1 (TmFLam ki ty pt f) where
  liftEq = $(makeLiftEq ''TmFLam)

instance (Ord1 f,  Monad f) => Ord1 (TmFLam ki ty pt f) where
  liftCompare = $(makeLiftCompare ''TmFLam)

instance (Show1 f) => Show1 (TmFLam ki ty pt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFLam)

instance EqRec (TmFLam ki ty pt) where
  liftEqRec eR e (TmLamF ty1 s1) (TmLamF ty2 s2) =
    liftEq eR ty1 ty2 && liftEqRec eR e s1 s2

instance OrdRec (TmFLam ki ty pt) where
  liftCompareRec cR c (TmLamF ty1 s1) (TmLamF ty2 s2) =
    case liftCompare cR ty1 ty2 of
      EQ -> liftCompareRec cR c s1 s2
      z -> z

instance ShowRec (TmFLam ki ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLamF ty sc) =
    showsBinaryWith (liftShowsPrec sR slR) (liftShowsPrecRec sR slR s sl) "TmLamF" n ty sc

instance Bound (TmFLam ki ty pt) where
  TmLamF Nothing s >>>= f = TmLamF Nothing (s >>>= f)
  TmLamF (Just ty) s >>>= f = TmLamF (Just (ty >>= f)) (s >>>= f)

instance Bitransversable (TmFLam ki ty pt) where
  bitransverse fT fL (TmLamF Nothing s) = TmLamF <$> pure Nothing <*> bitransverse fT fL s
  bitransverse fT fL (TmLamF (Just ty) s) = TmLamF <$> (Just <$> fT fL ty) <*> bitransverse fT fL s

class (TmAstBound ki ty pt tm, TmAstTransversable ki ty pt tm) => AsTmLam ki ty pt tm where
  _TmLamP :: Prism' (tm ki ty pt f a) (TmFLam ki ty pt f a)

  _TmLam :: Prism' (Term ki ty pt tm a) (Maybe (Type ki ty a), Scope () (TmAst ki ty pt tm) (TmAstVar a))
  _TmLam = _Wrapped . _TmAstTerm . _TmLamP . _TmLamF . mkPair (below _TmType) id

  _TmLamAnn :: Prism' (Term ki ty pt tm a) (Type ki ty a, Scope () (TmAst ki ty pt tm) (TmAstVar a))
  _TmLamAnn = _Wrapped . _TmAstTerm . _TmLamP . _TmLamF . mkPair (_Just . _TmType) id

  _TmLamNoAnn :: Prism' (Term ki ty pt tm a) (Scope () (TmAst ki ty pt tm) (TmAstVar a))
  _TmLamNoAnn =
    let
      capFst = iso
        (\((), x) -> x)
        (\x -> ((), x))
    in
      _Wrapped . _TmAstTerm . _TmLamP . _TmLamF . mkPair _Nothing id . capFst


instance (Bound ki, Bound (ty ki), Bound pt, Bitransversable ki, Bitransversable (ty ki), Bitransversable pt) => AsTmLam ki ty pt TmFLam where
  _TmLamP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki ty pt), Bitransversable (x ki ty pt), AsTmLam ki ty pt (TmSum xs)) => AsTmLam ki ty pt (TmSum (x ': xs)) where
  _TmLamP = _TmNext . _TmLamP

instance {-# OVERLAPPING #-} (Bound ki, Bound (ty ki), Bound pt, Bound (TmSum xs ki ty pt), Bitransversable ki, Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmLam ki ty pt (TmSum (TmFLam ': xs)) where
  _TmLamP = _TmNow . _TmLamP
