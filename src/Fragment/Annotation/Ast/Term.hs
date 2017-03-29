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
module Fragment.Annotation.Ast.Term (
    TmFAnnotation
  , AsTmAnnotation(..)
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

data TmFAnnotation (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmAnnotationF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFAnnotation

instance (Eq1 f, Monad f) => Eq1 (TmFAnnotation ki ty pt f) where
  liftEq = $(makeLiftEq ''TmFAnnotation)

instance (Ord1 f,  Monad f) => Ord1 (TmFAnnotation ki ty pt f) where
  liftCompare = $(makeLiftCompare ''TmFAnnotation)

instance (Show1 f) => Show1 (TmFAnnotation ki ty pt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFAnnotation)

instance EqRec (TmFAnnotation ki ty pt) where
  liftEqRec eR _ (TmAnnotationF ty1 tm1) (TmAnnotationF ty2 tm2) =
    eR ty1 ty2 && eR tm1 tm2

instance OrdRec (TmFAnnotation ki ty pt) where
  liftCompareRec cR _ (TmAnnotationF ty1 tm1) (TmAnnotationF ty2 tm2) =
    case cR ty1 ty2 of
      EQ -> cR tm1 tm2
      z -> z

instance ShowRec (TmFAnnotation ki ty pt) where
  liftShowsPrecRec sR _ _ _ n (TmAnnotationF ty tm) =
    showsBinaryWith sR sR "TmAnnotationF" n ty tm

instance Bound (TmFAnnotation ki ty pt) where
  TmAnnotationF ty tm >>>= f = TmAnnotationF (ty >>= f) (tm >>= f)

instance Bitransversable (TmFAnnotation ki ty pt) where
  bitransverse fT fL (TmAnnotationF ty tm) = TmAnnotationF <$> fT fL ty <*> fT fL tm

class (TmAstBound ki ty pt tm, TmAstTransversable ki ty pt tm) => AsTmAnnotation ki ty pt tm where
  _TmAnnotationP :: Prism' (tm ki ty pt f a) (TmFAnnotation ki ty pt f a)

  _TmAnnotation :: Prism' (Term ki ty pt tm a) (Type ki ty a, Term ki ty pt tm a)
  _TmAnnotation = _Wrapped . _TmAstTerm . _TmAnnotationP . _TmAnnotationF . mkPair _TmType _Unwrapped

instance (Bound ki, Bound (ty ki), Bound pt, Bitransversable ki, Bitransversable (ty ki), Bitransversable pt) => AsTmAnnotation ki ty pt TmFAnnotation where
  _TmAnnotationP = id

instance {-# OVERLAPPABLE #-} (Bound (x ki ty pt), Bitransversable (x ki ty pt), AsTmAnnotation ki ty pt (TmSum xs)) => AsTmAnnotation ki ty pt (TmSum (x ': xs)) where
  _TmAnnotationP = _TmNext . _TmAnnotationP

instance {-# OVERLAPPING #-} (Bound ki, Bound (ty ki), Bound pt, Bound (TmSum xs ki ty pt), Bitransversable ki, Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmAnnotation ki ty pt (TmSum (TmFAnnotation ': xs)) where
  _TmAnnotationP = _TmNow . _TmAnnotationP
