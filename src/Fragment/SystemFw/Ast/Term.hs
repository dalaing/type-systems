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
module Fragment.SystemFw.Ast.Term (
    TmFSystemFw
  , AsTmSystemFw(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith, showsBinaryWith)

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

data TmFSystemFw (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmLamF (k a) (Scope () k a)
  | TmAppF (k a) (k a)
  | TmLamTyF (Scope () k a)
  | TmAppTyF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFSystemFw

instance (Eq1 f, Monad f) => Eq1 (TmFSystemFw ki ty pt f) where
  liftEq = $(makeLiftEq ''TmFSystemFw)

instance (Ord1 f, Monad f) => Ord1 (TmFSystemFw ki ty pt f) where
  liftCompare = $(makeLiftCompare ''TmFSystemFw)

instance (Show1 f) => Show1 (TmFSystemFw ki ty pt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFSystemFw)

instance EqRec (TmFSystemFw ki ty pt) where
  liftEqRec eR e (TmLamF ty1 s1) (TmLamF ty2 s2) =
    eR ty1 ty2 && liftEqRec eR e s1 s2
  liftEqRec eR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec eR e (TmLamTyF s1) (TmLamTyF s2) =
    liftEqRec eR e s1 s2
  liftEqRec eR _ (TmAppTyF x1 y1) (TmAppTyF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFSystemFw ki ty pt) where
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

instance ShowRec (TmFSystemFw ki ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLamF ty sc) =
    showsBinaryWith sR (liftShowsPrecRec sR slR s sl) "TmLamF" n ty sc
  liftShowsPrecRec sR _ _ _ n (TmAppF x y) =
    showsBinaryWith sR sR "TmAppF" n x y
  liftShowsPrecRec sR slR s sl n (TmLamTyF sc) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TmLamTyF" n sc
  liftShowsPrecRec sR _ _ _ n (TmAppTyF x y) =
    showsBinaryWith sR sR "TmAppTyF" n x y

instance Bound (TmFSystemFw ki ty pt) where
  TmLamF ty s >>>= f = TmLamF (ty >>= f) (s >>>= f)
  TmAppF x y >>>= f = TmAppF (x >>= f) (y >>= f)
  TmLamTyF s >>>= f = TmLamTyF (s >>>= f)
  TmAppTyF x y >>>= f = TmAppTyF (x >>= f) (y >>= f)

instance Bitransversable (TmFSystemFw ki ty pt) where
  bitransverse fT fL (TmLamF ty s) = TmLamF <$> fT fL ty <*> bitransverse fT fL s
  bitransverse fT fL (TmAppF x y) = TmAppF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmLamTyF s) = TmLamTyF <$> bitransverse fT fL s
  bitransverse fT fL (TmAppTyF x y) = TmAppTyF <$> fT fL x <*> fT fL y

class (AstBound ki ty pt tm, AstTransversable ki ty pt tm) => AsTmSystemFw ki ty pt tm where
  _TmSystemFwP :: Prism' (tm ki ty pt f a) (TmFSystemFw ki ty pt f a)

  _TmSystemFwLink :: Prism' (Term ki ty pt tm a) (TmFSystemFw ki ty pt (Ast ki ty pt tm) (AstVar a))
  _TmSystemFwLink = _Wrapped . _ATerm . _TmSystemFwP

  _TmLam :: Prism' (Term ki ty pt tm a) (Type ki ty a, Scope () (Ast ki ty pt tm) (AstVar a))
  _TmLam = _TmSystemFwLink . _TmLamF . mkPair _Type id

  _TmApp :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Term ki ty pt tm a)
  _TmApp = _TmSystemFwLink . _TmAppF . mkPair _Unwrapped _Unwrapped

  _TmLamTy :: Prism' (Term ki ty pt tm a) (Scope () (Ast ki ty pt tm) (AstVar a))
  _TmLamTy = _TmSystemFwLink . _TmLamTyF

  _TmAppTy :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Type ki ty a)
  _TmAppTy = _TmSystemFwLink . _TmAppTyF . mkPair _Unwrapped _Type

instance (Bound (ty ki), Bound pt, Bitransversable (ty ki), Bitransversable pt) => AsTmSystemFw ki ty pt TmFSystemFw where
  _TmSystemFwP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki ty pt), Bitransversable (x ki ty pt), AsTmSystemFw ki ty pt (TmSum xs)) => AsTmSystemFw ki ty pt (TmSum (x ': xs)) where
  _TmSystemFwP = _TmNext . _TmSystemFwP

instance {-# OVERLAPPING #-} (Bound (ty ki), Bound pt, Bound (TmSum xs ki ty pt), Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmSystemFw ki ty pt (TmSum (TmFSystemFw ': xs)) where
  _TmSystemFwP = _TmNow . _TmSystemFwP
