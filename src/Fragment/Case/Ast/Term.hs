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
module Fragment.Case.Ast.Term (
    Alt(..)
  , TmFCase
  , AsTmCase(..)
  ) where

import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), eq1, compare1, showsPrec1, showsBinaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Iso (bimapping)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import qualified Data.List.NonEmpty as N

import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec
import Util.NonEmpty

data Alt (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
  Alt (k a) (Scope Int k a)
  deriving (Functor, Foldable, Traversable)

makePrisms ''Alt

instance (Eq1 tm, Monad tm) => Eq1 (Alt ki ty pt tm) where
  liftEq = $(makeLiftEq ''Alt)

instance (Ord1 tm, Monad tm) => Ord1 (Alt ki ty pt tm) where
  liftCompare = $(makeLiftCompare ''Alt)

instance (Show1 tm) => Show1 (Alt ki ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''Alt)

instance (Eq a, Eq1 tm, Monad tm) => Eq (Alt ki ty pt tm a) where (==) = eq1
instance (Ord a, Ord1 tm, Monad tm) => Ord (Alt ki ty pt tm a) where compare = compare1
instance (Show a, Show1 tm) => Show (Alt ki ty pt tm a) where showsPrec = showsPrec1

instance EqRec (Alt ki ty pt) where
  liftEqRec eR e (Alt pt1 tm1) (Alt pt2 tm2) =
    eR pt1 pt2 && liftEqRec eR e tm1 tm2

instance OrdRec (Alt ki ty pt) where
  liftCompareRec cR c (Alt pt1 tm1) (Alt pt2 tm2) =
    case cR pt1 pt2 of
      EQ -> liftCompareRec cR c tm1 tm2
      z -> z

instance ShowRec (Alt ki ty pt) where
  liftShowsPrecRec sR slR s sl n (Alt pt tm) =
    showsBinaryWith sR (liftShowsPrecRec sR slR s sl) "Alt" n pt tm

instance Bound (Alt ki ty pt) where
  Alt pt s >>>= f = Alt (pt >>= f) (s >>>= f)

instance Bitransversable (Alt ki ty pt) where
  bitransverse fT fL (Alt pt s) = Alt <$> fT fL pt <*> bitransverse fT fL s

data TmFCase (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmCaseF (f a) (NE (Alt ki ty pt f a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFCase

instance (Eq1 tm, Monad tm) => Eq1 (TmFCase ki ty pt tm) where
  liftEq = $(makeLiftEq ''TmFCase)

instance (Ord1 tm, Monad tm) => Ord1 (TmFCase ki ty pt tm) where
  liftCompare = $(makeLiftCompare ''TmFCase)

instance (Show1 tm) => Show1 (TmFCase ki ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFCase)

instance EqRec (TmFCase ki ty pt) where
  liftEqRec eR e (TmCaseF tm1 (NE alts1)) (TmCaseF tm2 (NE alts2)) =
    eR tm1 tm2 && and (N.zipWith (liftEqRec eR e) alts1 alts2)

instance OrdRec (TmFCase ki ty pt) where
  liftCompareRec cR c (TmCaseF tm1 (NE alts1)) (TmCaseF tm2 (NE alts2)) =
    let
      f [] [] = EQ
      f [] _ = LT
      f _ [] = GT
      f (x : xs) (y : ys) =
        case liftCompareRec cR c x y of
          EQ -> f xs ys
          z -> z
    in
      case cR tm1 tm2 of
        EQ -> f (N.toList alts1) (N.toList alts2)
        z -> z

instance ShowRec (TmFCase ki ty pt) where
  liftShowsPrecRec sR slR s sl n (TmCaseF tm (NE alts)) =
    showsBinaryWith sR (\_ -> liftShowListRec sR slR s sl) "TmCaseF" n tm (N.toList alts)

instance Bound (TmFCase ki ty pt) where
  TmCaseF tm alts >>>= f = TmCaseF (tm >>= f) (fmap (>>>= f) alts)

instance Bitransversable (TmFCase ki ty tp) where
  bitransverse fT fL (TmCaseF tm alts) = TmCaseF <$> fT fL tm <*> traverse (bitransverse fT fL) alts

class TmAstTransversable ki ty pt tm => AsTmCase ki ty pt tm where
  _TmCaseP :: Prism' (tm ki ty pt f a) (TmFCase ki ty pt f a)

  _TmCase :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, N.NonEmpty (Alt ki ty pt (TmAst ki ty pt tm) (TmAstVar a)))
  _TmCase = _Wrapped . _TmAstTerm. _TmCaseP . _TmCaseF . bimapping _Unwrapped _Wrapped

instance (Bitransversable ki, Bitransversable (ty ki), Bitransversable pt) => AsTmCase ki ty pt TmFCase where
  _TmCaseP = id

instance {-# OVERLAPPABLE #-} (Bitransversable (x ki ty pt), AsTmCase ki ty pt (TmSum xs)) => AsTmCase ki ty pt (TmSum (x ': xs)) where
  _TmCaseP = _TmNext . _TmCaseP

instance {-# OVERLAPPING #-} (Bitransversable ki, Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmCase ki ty pt (TmSum (TmFCase ': xs)) where
  _TmCaseP = _TmNow . _TmCaseP
