{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
module Fragment (
    ValueRule(..)
  , EvalRule(..)
  , InferRule(..)
  , FragmentInput(..)
  , FragmentOutput(..)
  , prepareFragment
  , mkCheck
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Error (AsUnexpected(..), expect, AsUnknownTypeError(..))

data ValueRule (ty :: * -> *) tm a =
    ValueBase (tm a -> Maybe (tm a))
  | ValueRecurse ((tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a))

fixValueRule :: (tm a -> Maybe (tm a))
             -> ValueRule ty tm a
             -> tm a
             -> Maybe (tm a )
fixValueRule _ (ValueBase f) = f
fixValueRule valueFn (ValueRecurse f) = f valueFn

mkValue :: [ValueRule ty tm a]
        -> tm a
        -> Maybe (tm a)
mkValue rules =
  let
    go tm = asum .
            fmap (\r -> fixValueRule go r tm) $
            rules
  in
    go

data EvalRule (ty :: * -> *) tm a =
    EvalBase (tm a -> Maybe (tm a))
  | EvalValue ((tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a))
  | EvalStep ((tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a))
  | EvalValueStep ((tm a -> Maybe (tm a)) -> (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a))

fixEvalRule :: (tm a -> Maybe (tm a))
            -> (tm a -> Maybe (tm a))
            -> EvalRule ty tm a
            -> tm a
            -> Maybe (tm a )
fixEvalRule _ _ (EvalBase f) = f
fixEvalRule valueFn _ (EvalValue f) = f valueFn
fixEvalRule _ evalFn (EvalStep f) = f evalFn
fixEvalRule valueFn evalFn (EvalValueStep f) = f valueFn evalFn

mkStep :: (tm a -> Maybe (tm a))
       -> [EvalRule ty tm a]
       -> tm a
       -> Maybe (tm a)
mkStep valueFn rules =
  let
    stepFn tm = asum .
            fmap (\r -> fixEvalRule valueFn stepFn r tm) $
            rules
  in
    stepFn

mkEval :: (tm a -> Maybe (tm a)) -> tm a -> tm a
mkEval stepFn =
  let
    go tm = case stepFn tm of
      Just tm' -> go tm'
      Nothing -> tm
  in
    go

data InferRule e s r m ty tm a =
    InferBase (tm a -> Maybe (m (ty a)))
  | InferRecurse ((tm a -> m (ty a)) -> tm a -> Maybe (m (ty a)))

fixInferRule :: (tm a -> m (ty a))
             -> InferRule e s r m ty tm a
             -> tm a
             -> Maybe (m (ty a))
fixInferRule _ (InferBase f) = f
fixInferRule inferFn (InferRecurse f) = f inferFn

mkInfer :: (MonadError e m, AsUnknownTypeError e) => [InferRule e s r m ty tm a] -> tm a -> m (ty a)
mkInfer rules =
  let
    go tm =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixInferRule go r tm) $
      rules
  in
    go

mkCheck :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a)) => (tm a -> m (ty a)) -> tm a -> ty a -> m ()
mkCheck inferFn =
  let
    go tm ty = do
      tyAc <- inferFn tm
      expect tyAc ty
  in
    go

data FragmentInput e s r m ty tm a =
  FragmentInput {
    fiValueRules :: [ValueRule ty tm a]
  , fiEvalRules :: [EvalRule ty tm a]
  , fiInferRules :: [InferRule e s r m ty tm a]
  }

instance Monoid (FragmentInput e s r m ty tm a) where
  mempty =
    FragmentInput mempty mempty mempty
  mappend (FragmentInput v1 e1 i1) (FragmentInput v2 e2 i2) =
    FragmentInput (mappend v1 v2) (mappend e1 e2) (mappend i1 i2)

data FragmentOutput e s r m ty tm a =
  FragmentOutput {
    foValue :: tm a -> Maybe (tm a)
  , foStep :: tm a -> Maybe (tm a)
  , foEval :: tm a -> tm a
  , foInfer :: tm a -> m (ty a)
  , foCheck :: tm a -> ty a -> m ()
  }

prepareFragment :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsUnknownTypeError e) => FragmentInput e s r m ty tm a -> FragmentOutput e s r m ty tm a
prepareFragment fi =
  let
    v = mkValue . fiValueRules $ fi
    s = mkStep v . fiEvalRules $ fi
    e = mkEval s
    i = mkInfer . fiInferRules $ fi
    c = mkCheck i
  in
    FragmentOutput v s e i c
