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

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith, showsBinaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Type
import Ast.Term
import Util

data TmFSystemF (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmLamF (k a) (Scope () k a)
  | TmAppF (k a) (k a)
  | TmLamTyF (Scope () k a)
  | TmAppTyF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFSystemF

instance (Eq1 k, Monad k) => Eq1 (TmFSystemF ty pt k) where
  liftEq = $(makeLiftEq ''TmFSystemF)

instance (Ord1 k,  Monad k) => Ord1 (TmFSystemF ty pt k) where
  liftCompare = $(makeLiftCompare ''TmFSystemF)

instance (Show1 k) => Show1 (TmFSystemF ty pt k) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFSystemF)

instance EqRec (TmFSystemF ty pt) where
  liftEqRec eR e (TmLamF ty1 s1) (TmLamF ty2 s2) =
    eR ty1 ty2 && liftEqRec eR e s1 s2
  liftEqRec eR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec eR e (TmLamTyF s1) (TmLamTyF s2) =
    liftEqRec eR e s1 s2
  liftEqRec eR _ (TmAppTyF x1 y1) (TmAppTyF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFSystemF ty pt) where
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
  liftCompareRec _ _ (TmAppF _ _) _ = LT
  liftCompareRec _ _ _ (TmAppF _ _) = GT
  liftCompareRec cR c (TmLamTyF s1) (TmLamTyF s2) =
    liftCompareRec cR c s1 s2
  liftCompareRec _ _ (TmLamTyF _) _ = LT
  liftCompareRec _ _ _ (TmLamTyF _) = GT
  liftCompareRec cR _ (TmAppTyF x1 y1) (TmAppTyF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFSystemF ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLamF ty sc) =
    showsBinaryWith sR (liftShowsPrecRec sR slR s sl) "TmLamF" n ty sc
  liftShowsPrecRec sR _ _ _ n (TmAppF x y) =
    showsBinaryWith sR sR "TmAppF" n x y
  liftShowsPrecRec sR slR s sl n (TmLamTyF sc) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TmLamTyF" n sc
  liftShowsPrecRec sR _ _ _ n (TmAppTyF x y) =
    showsBinaryWith sR sR "TmAppTyF" n x y

instance Bound (TmFSystemF ty pt) where
  TmLamF ty s >>>= f = TmLamF (ty >>= f) (s >>>= f)
  TmAppF x y >>>= f = TmAppF (x >>= f) (y >>= f)
  TmLamTyF s >>>= f = TmLamTyF (s >>>= f)
  TmAppTyF x y >>>= f = TmAppTyF (x >>= f) (y >>= f)

instance Bitransversable (TmFSystemF ty pt) where
  bitransverse fT fL (TmLamF ty s) = TmLamF <$> fT fL ty <*> bitransverse fT fL s
  bitransverse fT fL (TmAppF x y) = TmAppF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmLamTyF s) = TmLamTyF <$> bitransverse fT fL s
  bitransverse fT fL (TmAppTyF x y) = TmAppTyF <$> fT fL x <*> fT fL y

class (AstBound ty pt tm, AstTransversable ty pt tm) => AsTmSystemF ty pt tm where
  _TmSystemFP :: Prism' (tm ty pt k a) (TmFSystemF ty pt k a)

  _TmSystemFLink :: Prism' (Term ty pt tm a) (TmFSystemF ty pt (Ast ty pt tm) (AstVar a))
  _TmSystemFLink = _Wrapped . _ATerm . _TmSystemFP

  _TmLam :: Prism' (Term ty pt tm a) (Type ty a, Scope () (Ast ty pt tm) (AstVar a))
  _TmLam = _TmSystemFLink . _TmLamF . mkPair _Type id

  _TmApp :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmApp = _TmSystemFLink . _TmAppF . mkPair _Unwrapped _Unwrapped

  _TmLamTy :: Prism' (Term ty pt tm a) (Scope () (Ast ty pt tm) (AstVar a))
  _TmLamTy = _TmSystemFLink . _TmLamTyF

  _TmAppTy :: Prism' (Term ty pt tm a) (Term ty pt tm a, Type ty a)
  _TmAppTy = _TmSystemFLink . _TmAppTyF . mkPair _Unwrapped _Type

instance (Bound ty, Bound pt, Bitransversable ty, Bitransversable pt) => AsTmSystemF ty pt TmFSystemF where
  _TmSystemFP = id

instance {-# OVERLAPPABLE #-} (Bound (x ty pt), Bitransversable (x ty pt), AsTmSystemF ty pt (TmSum xs)) => AsTmSystemF ty pt (TmSum (x ': xs)) where
  _TmSystemFP = _TmNext . _TmSystemFP

instance {-# OVERLAPPING #-} (Bound ty, Bound pt, Bound (TmSum xs ty pt), Bitransversable ty, Bitransversable pt, Bitransversable (TmSum xs ty pt)) => AsTmSystemF ty pt (TmSum (TmFSystemF ': xs)) where
  _TmSystemFP = _TmNow . _TmSystemFP
