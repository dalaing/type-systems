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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Case (
  ) where

import Data.List (elemIndex)
import Data.Foldable (toList)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N

import qualified Data.Text as T

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment

-- case is Case (tm a) [alt a]
-- alt is Alt (p a) (Scope Int tm a)

data Alt p tm a =
  Alt (p T.Text) (Scope Int tm a)
  deriving (Functor, Foldable, Traversable)

tmAlt :: (Foldable p, Monad tm) => p T.Text -> tm T.Text -> Alt p tm T.Text
tmAlt p tm = Alt p s
  where
    vs = toList p
    s = abstract (`elemIndex` vs) tm

makePrisms ''Alt

instance (Eq1 p, Eq (p T.Text), Eq1 tm, Monad tm) => Eq1 (Alt p tm) where
  liftEq = $(makeLiftEq ''Alt)

instance (Ord1 p, Ord (p T.Text), Ord1 tm, Monad tm) => Ord1 (Alt p tm) where
  liftCompare = $(makeLiftCompare ''Alt)

instance (Show1 p, Show (p T.Text), Show1 tm, Monad tm) => Show1 (Alt p tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''Alt)

instance (Eq1 p, Eq (p T.Text), Eq1 tm, Monad tm, Eq a) => Eq (Alt p tm a) where (==) = eq1
instance (Ord1 p, Ord (p T.Text), Ord1 tm, Monad tm, Ord a) => Ord (Alt p tm a) where compare = compare1
instance (Show1 p, Show (p T.Text), Show1 tm, Monad tm, Show a) => Show (Alt p tm a) where showsPrec = showsPrec1

instance Bound (Alt p) where
  Alt p s >>>= f = Alt p (s >>>= f)

data TmFCase p tm a =
    TmCaseF (tm a) (N.NonEmpty (Alt p tm a))
  deriving (Functor, Foldable, Traversable)

-- need to evaluate tm to a value fst
dodgyMatch :: Monad tm => (p T.Text -> tm T.Text -> Maybe [tm T.Text])  -> TmFCase p tm T.Text -> tm T.Text
dodgyMatch matchFn (TmCaseF tm alts) =
  let
    go (Alt p s : rest) = case matchFn p tm of
      Nothing -> go rest
      Just tms -> instantiate (tms !!) s
  in
    go (N.toList alts)

dodgyCheck :: (tm T.Text -> m (ty T.Text)) -> (p T.Text -> ty T.Text -> m [ty T.Text]) -> TmFCase p tm T.Text -> m (ty T.Text)
dodgyCheck inferFn checkFn (TmCaseF tm alts) =
  let
    go ty (Alt p s) = do
      tys <- checkFn p ty
      -- TODO generate fresh vars, add tys to context with those vars,
      -- add vars to s, and infer
      infer _
  in
    do
      ty <- inferFn tm
      tys <- mapM (go ty) (N.toList alts)
      expectAllEq tys


-- TODO replace with typeclass
-- tmCase :: tm a -> N.NonEmpty (Alt p tm a) -> tm a
-- tmCase = curry $ review _TmCase

makePrisms ''TmFCase

deriveEq1 ''NonEmpty
deriveOrd1 ''NonEmpty
deriveShow1 ''NonEmpty

instance (Eq1 p, Eq (p T.Text), Eq1 tm, Monad tm) => Eq1 (TmFCase p tm) where
  liftEq = $(makeLiftEq ''TmFCase)

instance (Ord1 p, Ord (p T.Text), Ord1 tm, Monad tm) => Ord1 (TmFCase p tm) where
  liftCompare = $(makeLiftCompare ''TmFCase)

instance (Show1 p, Show (p T.Text), Show1 tm, Monad tm) => Show1 (TmFCase p tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFCase)

instance (Eq1 p, Eq (p T.Text), Eq1 tm, Monad tm, Eq a) => Eq (TmFCase p tm a) where (==) = eq1
instance (Ord1 p, Ord (p T.Text), Ord1 tm, Monad tm, Ord a) => Ord (TmFCase p tm a) where compare = compare1
instance (Show1 p, Show (p T.Text), Show1 tm, Monad tm, Show a) => Show (TmFCase p tm a) where showsPrec = showsPrec1

instance Bound (TmFCase p) where
  TmCaseF tm alts >>>= f = TmCaseF (tm >>= f) (fmap (>>>= f) alts)

class AsTmCase p tm | tm -> p where
  _TmCaseP :: Prism' (tm a) (TmFCase p tm a)

  _TmCase :: Prism' (tm a) (tm a, N.NonEmpty (Alt p tm a))
  _TmCase = _TmCaseP . _TmCaseF

instance AsTmCase p tm => AsTmCase p (TmFCase p tm) where
  _TmCaseP = id . _TmCaseP
