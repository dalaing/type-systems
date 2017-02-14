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
  , PMatchRule(..)
  , PCheckRule(..)
  , FragmentInput(..)
  , FragmentOutput(..)
  , prepareFragment
  , mkCheck
  ) where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

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

data PMatchRule p tm a =
    PMatchBase (p a -> tm a -> Maybe [tm a])
  | PMatchRecurse ((p a -> tm a -> Maybe [tm a]) -> p a -> tm a -> Maybe [tm a])

fixPMatchRule :: (p a -> tm a -> Maybe [tm a]) -> PMatchRule p tm a -> p a -> tm a -> Maybe [tm a]
fixPMatchRule _ (PMatchBase f) = f
fixPMatchRule pMatchFn (PMatchRecurse f) = f pMatchFn

mkPMatch :: [PMatchRule p tm a] -> p a -> tm a -> Maybe [tm a]
mkPMatch rules x y =
  let
    go p tm =
      asum .
      fmap (\r -> fixPMatchRule go r p tm) $
      rules
  in
    fmap reverse $ go x y

data PCheckRule e m p ty a =
    PCheckBase (p a -> ty a -> Maybe (m [ty a]))
  | PCheckRecurse ((p a -> ty a -> m [ty a]) -> p a -> ty a -> Maybe (m [ty a]))

fixPCheckRule :: (p a -> ty a -> m [ty a]) -> PCheckRule e m p ty a -> p a -> ty a -> Maybe (m [ty a])
fixPCheckRule _ (PCheckBase f) = f
fixPCheckRule pCheckFn (PCheckRecurse f) = f pCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e) => [PCheckRule e m p ty a] -> p a -> ty a -> m [ty a]
mkPCheck rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule go r p ty) $
      rules
  in
    fmap reverse $ go x y

-- TODO split matching and finding bindings
-- - matching is done at run time, we want to find bindings at compile time
-- - use Data.Sequence for the bindings

data FragmentInput e s r m ty p tm a =
  FragmentInput {
    fiValueRules :: [ValueRule ty tm a]
  , fiEvalRules :: [EvalRule ty tm a]
  , fiInferRules :: [InferRule e s r m ty tm a]
  , fiPMatchRules :: [PMatchRule p tm a]
  , fiPCheckRules :: [PCheckRule e m p ty a]
  }

instance Monoid (FragmentInput e s r m ty p tm a) where
  mempty =
    FragmentInput mempty mempty mempty mempty mempty
  mappend (FragmentInput v1 e1 i1 m1 c1) (FragmentInput v2 e2 i2 m2 c2) =
    FragmentInput
      (mappend v1 v2)
      (mappend e1 e2)
      (mappend i1 i2)
      (mappend m1 m2)
      (mappend c1 c2)

data FragmentOutput e s r m ty p tm a =
  FragmentOutput {
    foValue :: tm a -> Maybe (tm a)
  , foStep :: tm a -> Maybe (tm a)
  , foEval :: tm a -> tm a
  , foInfer :: tm a -> m (ty a)
  , foCheck :: tm a -> ty a -> m ()
  , foPMatch :: p a -> tm a -> Maybe [tm a]
  , foPCheck :: p a -> ty a -> m [ty a]
  }

prepareFragment :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsUnknownTypeError e) => FragmentInput e s r m ty p tm a -> FragmentOutput e s r m ty p tm a
prepareFragment fi =
  let
    v = mkValue . fiValueRules $ fi
    s = mkStep v . fiEvalRules $ fi
    e = mkEval s
    i = mkInfer . fiInferRules $ fi
    c = mkCheck i
    pm = mkPMatch . fiPMatchRules $ fi
    pc = mkPCheck . fiPCheckRules $ fi
  in
    FragmentOutput v s e i c pm pc
