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
module Fragment.TmApp.Ast.Term (
    TmFApp
  , AsTmApp(..)
  ) where

import Data.Functor.Classes (showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TmFApp (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
  TmAppF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFApp

deriveEq1 ''TmFApp
deriveOrd1 ''TmFApp
deriveShow1 ''TmFApp

instance EqRec (TmFApp ki ty pt) where
  liftEqRec eR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    eR x1 x2 && eR y1 y2

instance OrdRec (TmFApp ki ty pt) where
  liftCompareRec cR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFApp ki ty pt) where
  liftShowsPrecRec sR _ _ _ n (TmAppF x y) =
    showsBinaryWith sR sR "TmAppF" n x y

instance Bound (TmFApp ki ty pt) where
  TmAppF x y >>>= f = TmAppF (x >>= f) (y >>= f)

instance Bitransversable (TmFApp ki ty pt) where
  bitransverse fT fL (TmAppF x y) = TmAppF <$> fT fL x <*> fT fL y

class (TmAstBound ki ty pt tm, TmAstTransversable ki ty pt tm) => AsTmApp ki ty pt tm where
  _TmAppP :: Prism' (tm ki ty pt f a) (TmFApp ki ty pt f a)

  _TmApp :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Term ki ty pt tm a)
  _TmApp = _Wrapped . _TmAstTerm . _TmAppP . _TmAppF . mkPair _Unwrapped _Unwrapped

instance (Bound ki, Bound (ty ki), Bound pt, Bitransversable ki, Bitransversable (ty ki), Bitransversable pt) => AsTmApp ki ty pt TmFApp where
  _TmAppP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki ty pt), Bitransversable (x ki ty pt), AsTmApp ki ty pt (TmSum xs)) => AsTmApp ki ty pt (TmSum (x ': xs)) where
  _TmAppP = _TmNext . _TmAppP

instance {-# OVERLAPPING #-} (Bound ki, Bound (ty ki), Bound pt, Bound (TmSum xs ki ty pt), Bitransversable ki, Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmApp ki ty pt (TmSum (TmFApp ': xs)) where
  _TmAppP = _TmNow . _TmAppP
