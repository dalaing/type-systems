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
module Fragment.Let.Ast.Term (
    LetBinding(..)
  , TmFLet
  , AsTmLet(..)
  ) where

import Data.Functor.Classes (Eq1(..), eq1, Ord1(..), compare1, Show1(..), showsPrec1, showsBinaryWith)

import Bound (Bound(..), Scope)
import Control.Lens.Iso (bimapping)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec

data LetBinding (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
  LetBinding (Maybe (k a)) (Scope Int k a)
  deriving (Functor, Foldable, Traversable)

makePrisms ''LetBinding

instance (Eq1 tm, Monad tm) => Eq1 (LetBinding ki ty pt tm) where
  liftEq = $(makeLiftEq ''LetBinding)

instance (Ord1 tm, Monad tm) => Ord1 (LetBinding ki ty pt tm) where
  liftCompare = $(makeLiftCompare ''LetBinding)

instance (Show1 tm) => Show1 (LetBinding ki ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''LetBinding)

instance (Eq a, Eq1 tm, Monad tm) => Eq (LetBinding ki ty pt tm a) where (==) = eq1
instance (Ord a, Ord1 tm, Monad tm) => Ord (LetBinding ki ty pt tm a) where compare = compare1
instance (Show a, Show1 tm) => Show (LetBinding ki ty pt tm a) where showsPrec = showsPrec1

instance EqRec (LetBinding ki ty pt) where
  liftEqRec eR e (LetBinding ty1 tm1) (LetBinding ty2 tm2) =
    liftEq eR ty1 ty2 && liftEqRec eR e tm1 tm2

instance OrdRec (LetBinding ki ty pt) where
  liftCompareRec cR c (LetBinding ty1 tm1) (LetBinding ty2 tm2) =
    case liftCompare cR ty1 ty2 of
      EQ -> liftCompareRec cR c tm1 tm2
      z -> z

instance ShowRec (LetBinding ki ty pt) where
  liftShowsPrecRec sR slR s sl n (LetBinding ty tm) =
    showsBinaryWith (liftShowsPrec sR slR) (liftShowsPrecRec sR slR s sl) "LetBinding" n ty tm

instance Bound (LetBinding ki ty pt) where
  LetBinding Nothing s >>>= f = LetBinding Nothing (s >>>= f)
  LetBinding (Just ty) s >>>= f = LetBinding (Just (ty >>= f)) (s >>>= f)

instance Bitransversable (LetBinding ki ty pt) where
  bitransverse fT fL (LetBinding Nothing s) = LetBinding <$> pure Nothing <*> bitransverse fT fL s
  bitransverse fT fL (LetBinding (Just ty) s) = LetBinding <$> (Just <$> fT fL ty) <*> bitransverse fT fL s

data TmFLet (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmLetF [LetBinding ki ty pt f a] (Scope Int f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFLet

instance (Eq1 tm, Monad tm) => Eq1 (TmFLet ki ty pt tm) where
  liftEq = $(makeLiftEq ''TmFLet)

instance (Ord1 tm, Monad tm) => Ord1 (TmFLet ki ty pt tm) where
  liftCompare = $(makeLiftCompare ''TmFLet)

instance (Show1 tm) => Show1 (TmFLet ki ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFLet)

instance EqRec (TmFLet ki ty pt) where
  liftEqRec eR e (TmLetF binds1 tm1) (TmLetF binds2 tm2) =
    and (zipWith (liftEqRec eR e) binds1 binds2) && liftEqRec eR e tm1 tm2

instance OrdRec (TmFLet ki ty pt) where
  liftCompareRec cR c (TmLetF binds1 tm1) (TmLetF binds2 tm2) =
    let
      f [] [] = EQ
      f [] _ = LT
      f _ [] = GT
      f (x : xs) (y : ys) =
        case liftCompareRec cR c x y of
          EQ -> f xs ys
          z -> z
    in
      case f binds1 binds2 of
        EQ -> liftCompareRec cR c tm1 tm2
        z -> z

instance ShowRec (TmFLet ki ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLetF binds tm) =
    showsBinaryWith (\_ -> liftShowListRec sR slR s sl) (liftShowsPrecRec sR slR s sl) "TmLetF" n binds tm

instance Bound (TmFLet ki ty pt) where
  TmLetF binds tm >>>= f = TmLetF (fmap (>>>= f) binds) (tm >>>= f)

instance Bitransversable (TmFLet ki ty tp) where
  bitransverse fT fL (TmLetF binds tm) = TmLetF <$> traverse (bitransverse fT fL) binds <*> bitransverse fT fL tm

class TmAstTransversable ki ty pt tm => AsTmLet ki ty pt tm where
  _TmLetP :: Prism' (tm ki ty pt f a) (TmFLet ki ty pt f a)

  _TmLet :: Prism' (Term ki ty pt tm a) ([LetBinding ki ty pt (TmAst ki ty pt tm) (TmAstVar a)], Scope Int (TmAst ki ty pt tm) (TmAstVar a))
  _TmLet = _Wrapped . _TmAstTerm. _TmLetP . _TmLetF . bimapping id id

instance (Bitransversable ki, Bitransversable (ty ki), Bitransversable pt) => AsTmLet ki ty pt TmFLet where
  _TmLetP = id

instance {-# OVERLAPPABLE #-} (Bitransversable (x ki ty pt), AsTmLet ki ty pt (TmSum xs)) => AsTmLet ki ty pt (TmSum (x ': xs)) where
  _TmLetP = _TmNext . _TmLetP

instance {-# OVERLAPPING #-} (Bitransversable ki, Bitransversable (ty ki), Bitransversable pt, Bitransversable (TmSum xs ki ty pt)) => AsTmLet ki ty pt (TmSum (TmFLet ': xs)) where
  _TmLetP = _TmNow . _TmLetP
