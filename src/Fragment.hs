{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment (
    ValueRule(..)
  , EvalRule(..)
  , InferRule(..)
  , PMatchRule(..)
  , PCheckRule(..)
  , PAddRule(..)
  , PUncoveredRule(..)
  , FragmentInput(..)
  , FragmentOutput(..)
  , prepareFragmentStrict
  , prepareFragmentLazy
  , mkCheck
  ) where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Fragment.Ast
import Error (AsUnexpected(..), expect, AsUnknownTypeError(..))
import Util

data ValueRule ty pt tm a =
    ValueBase (Term ty pt tm a -> Maybe (Term ty pt tm a))
  | ValueRecurse ((Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a))

fixValueRule :: (Term ty pt tm a -> Maybe (Term ty pt tm a))
             -> ValueRule ty pt tm a
             -> Term ty pt tm a
             -> Maybe (Term ty pt tm a )
fixValueRule _ (ValueBase f) = f
fixValueRule valueFn (ValueRecurse f) = f valueFn

mkValue :: [ValueRule ty pt tm a]
        -> Term ty pt tm a
        -> Maybe (Term ty pt tm a)
mkValue rules =
  let
    go tm = asum .
            fmap (\r -> fixValueRule go r tm) $
            rules
  in
    go

data EvalRule ty pt tm a =
    EvalBase (Term ty pt tm a -> Maybe (Term ty pt tm a))
  | EvalValue ((Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a))
  | EvalStep ((Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a))
  | EvalPMatch ((Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Term ty pt tm a -> Maybe (Term ty pt tm a))
  | EvalValuePMatch ((Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Term ty pt tm a -> Maybe (Term ty pt tm a))
  | EvalValueStep ((Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a))

fixEvalRule :: (Term ty pt tm a -> Maybe (Term ty pt tm a))
            -> (Term ty pt tm a -> Maybe (Term ty pt tm a))
            -> (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])
            -> EvalRule ty pt tm a
            -> Term ty pt tm a
            -> Maybe (Term ty pt tm a )
fixEvalRule _ _ _ (EvalBase f) = f
fixEvalRule valueFn _ _ (EvalValue f) = f valueFn
fixEvalRule _ evalFn _ (EvalStep f) = f evalFn
fixEvalRule _ _ matchFn (EvalPMatch f) = f matchFn
fixEvalRule valueFn _ matchFn (EvalValuePMatch f) = f valueFn matchFn
fixEvalRule valueFn evalFn _ (EvalValueStep f) = f valueFn evalFn

mkStep :: (Term ty pt tm a -> Maybe (Term ty pt tm a))
       -> (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])
       -> [EvalRule ty pt tm a]
       -> Term ty pt tm a
       -> Maybe (Term ty pt tm a)
mkStep valueFn matchFn rules =
  let
    stepFn tm = asum .
            fmap (\r -> fixEvalRule valueFn stepFn matchFn r tm) $
            rules
  in
    stepFn

mkEval :: (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Term ty pt tm a
mkEval stepFn =
  let
    go tm = case stepFn tm of
      Just tm' -> go tm'
      Nothing -> tm
  in
    go

data InferRule e s r m ty pt tm a =
    InferBase (Term ty pt tm a -> Maybe (m (Type ty a)))
  | InferPCheck ((Term ty pt tm a -> m (Type ty a)) -> (Pattern pt a -> Type ty a -> m [Type ty a]) -> Term ty pt tm a -> Maybe (m (Type ty a)))
  | InferRecurse ((Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a)))

fixInferRule :: (Term ty pt tm a -> m (Type ty a))
             -> (Pattern pt a -> Type ty a -> m [Type ty a])
             -> InferRule e s r m ty pt tm a
             -> Term ty pt tm a
             -> Maybe (m (Type ty a))
fixInferRule _ _ (InferBase f) = f
fixInferRule inferFn checkFn (InferPCheck f) = f inferFn checkFn
fixInferRule inferFn _ (InferRecurse f) = f inferFn

mkInfer :: (MonadError e m, AsUnknownTypeError e) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> [InferRule e s r m ty pt tm a] -> Term ty pt tm a -> m (Type ty a)
mkInfer pc rules =
  let
    go tm =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixInferRule go pc r tm) $
      rules
  in
    go

mkCheck :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Type ty a -> m ()
mkCheck inferFn =
  let
    go tm ty = do
      tyAc <- inferFn tm
      expect tyAc ty
  in
    go

data PMatchRule ty pt tm a =
    PMatchBase (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])
  | PMatchEval ((Term ty pt tm a -> Term ty pt tm a) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])
  | PMatchRecurse ((Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])

fixPMatchRule :: (Term ty pt tm a -> Term ty pt tm a) -> (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> PMatchRule ty pt tm a -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
fixPMatchRule _ _ (PMatchBase f) = f
fixPMatchRule evalFn _ (PMatchEval f) = f evalFn
fixPMatchRule _ pMatchFn (PMatchRecurse f) = f pMatchFn

mkPMatch :: (Term ty pt tm a -> Term ty pt tm a) -> [PMatchRule ty pt tm a] -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
mkPMatch innerEval rules x y =
  let
    go p tm =
      asum .
      fmap (\r -> fixPMatchRule innerEval go r p tm) $
      rules
  in
    go x y

data PCheckRule e m pt ty a =
    PCheckBase (Pattern pt a -> Type ty a -> Maybe (m [Type ty a]))
  | PCheckRecurse ((Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a]))

fixPCheckRule :: (Pattern pt a -> Type ty a -> m [Type ty a]) -> PCheckRule e m pt ty a -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
fixPCheckRule _ (PCheckBase f) = f
fixPCheckRule pCheckFn (PCheckRecurse f) = f pCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e) => [PCheckRule e m pt ty a] -> Pattern pt a -> Type ty a -> m [Type ty a]
mkPCheck rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule go r p ty) $
      rules
  in
    go x y

data PAddRule (pt :: (* -> *) -> * -> *) a = PAddRule

mkAdd :: [PAddRule pt a] -> ()
mkAdd _ = ()

data PUncoveredRule (pt :: (* -> *) -> * -> *) a = PUncoveredRule

mkUncovered :: [PUncoveredRule pt a] -> ()
mkUncovered _ = ()

data FragmentInput e s r m ty pt tm a =
  FragmentInput {
    fiValueRules :: [ValueRule ty pt tm a]
  , fiEvalRules :: [EvalRule ty pt tm a]
  , fiInferRules :: [InferRule e s r m ty pt tm a]
  , fiPMatchRules :: [PMatchRule ty pt tm a]
  , fiPCheckRules :: [PCheckRule e m pt ty a]
  , fiPAddRules :: [PAddRule pt a]
  , fiPUncoveredRules :: [PUncoveredRule pt a]
  }

instance Monoid (FragmentInput e s r m ty pt tm a) where
  mempty =
    FragmentInput mempty mempty mempty mempty mempty mempty mempty
  mappend (FragmentInput v1 e1 i1 m1 c1 a1 u1) (FragmentInput v2 e2 i2 m2 c2 a2 u2) =
    FragmentInput
      (mappend v1 v2)
      (mappend e1 e2)
      (mappend i1 i2)
      (mappend m1 m2)
      (mappend c1 c2)
      (mappend a1 a2)
      (mappend u1 u2)

data FragmentOutput e s r m ty pt tm a =
  FragmentOutput {
    foValue :: Term ty pt tm a -> Maybe (Term ty pt tm a)
  , foStep :: Term ty pt tm a -> Maybe (Term ty pt tm a)
  , foEval :: Term ty pt tm a -> Term ty pt tm a
  , foInfer :: Term ty pt tm a -> m (Type ty a)
  , foCheck :: Term ty pt tm a -> Type ty a -> m ()
  , foPMatch :: Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
  , foPCheck :: Pattern pt a -> Type ty a -> m [Type ty a]
  , foPAdd :: ()
  , foPUncovered :: ()
  }

prepareFragment :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsUnknownTypeError e)
                => (Term ty pt tm a -> Term ty pt tm a)
                -> FragmentInput e s r m ty pt tm a
                -> FragmentOutput e s r m ty pt tm a
prepareFragment innerMatchEval fi =
  let
    v = mkValue . fiValueRules $ fi
    s = mkStep v pm . fiEvalRules $ fi
    e = mkEval s
    i = mkInfer pc . fiInferRules $ fi
    c = mkCheck i
    pm = mkPMatch innerMatchEval . fiPMatchRules $ fi
    pc = mkPCheck . fiPCheckRules $ fi
    a = mkAdd . fiPAddRules $ fi
    u = mkUncovered . fiPUncoveredRules $ fi
  in
    FragmentOutput v s e i c pm pc a u

prepareFragmentStrict :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsUnknownTypeError e)
                => FragmentInput e s r m ty pt tm a
                -> FragmentOutput e s r m ty pt tm a
prepareFragmentStrict = prepareFragment id

prepareFragmentLazy :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsUnknownTypeError e)
                => FragmentInput e s r m ty pt tm a
                -> FragmentOutput e s r m ty pt tm a
prepareFragmentLazy fi =
  let
    fo = prepareFragment e fi
    e = foEval fo
  in
    fo
