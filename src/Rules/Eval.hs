{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Rules.Eval (
    ValueRule(..)
  , EvalRule(..)
  , MatchRule(..)
  , EvalInput(..)
  , EvalOutput(..)
  , EvalContext
  , prepareEvalLazy
  , prepareEvalStrict
  ) where

-- TODO it might be worth having both lazy and strict eval rules in the one structure
-- - this is so we can play with primitives that switch the eval mode between lazy and strict
-- - for bonus points, we could annotate types and functions as strict / lazy / marked with an evaluation variable which can participate in unification etc..

import Data.Foldable (asum)
import GHC.Exts (Constraint)

import Ast.Term
import Ast.Pattern

data ValueRule ki ty pt tm a =
    ValueBase (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
  | ValueRecurse ((Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))

fixValueRule :: (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
             -> ValueRule ki ty pt tm a
             -> Term ki ty pt tm a
             -> Maybe (Term ki ty pt tm a )
fixValueRule _ (ValueBase f) = f
fixValueRule valueFn (ValueRecurse f) = f valueFn

mkValue :: [ValueRule ki ty pt tm a]
        -> Term ki ty pt tm a
        -> Maybe (Term ki ty pt tm a)
mkValue rules =
  let
    go tm = asum .
            fmap (\r -> fixValueRule go r tm) $
            rules
  in
    go

data EvalRule ki ty pt tm a =
    EvalBase (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
  | EvalValue ((Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
  | EvalStep ((Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
  | EvalMatch ((Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
  | EvalValueMatch ((Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
  | EvalValueStep ((Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))

fixEvalRule :: (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
            -> (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
            -> (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a])
            -> EvalRule ki ty pt tm a
            -> Term ki ty pt tm a
            -> Maybe (Term ki ty pt tm a )
fixEvalRule _ _ _ (EvalBase f) = f
fixEvalRule valueFn _ _ (EvalValue f) = f valueFn
fixEvalRule _ evalFn _ (EvalStep f) = f evalFn
fixEvalRule _ _ matchFn (EvalMatch f) = f matchFn
fixEvalRule valueFn _ matchFn (EvalValueMatch f) = f valueFn matchFn
fixEvalRule valueFn evalFn _ (EvalValueStep f) = f valueFn evalFn

mkStep :: (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
       -> (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a])
       -> [EvalRule ki ty pt tm a]
       -> Term ki ty pt tm a
       -> Maybe (Term ki ty pt tm a)
mkStep valueFn matchFn rules =
  let
    stepFn tm = asum .
            fmap (\r -> fixEvalRule valueFn stepFn matchFn r tm) $
            rules
  in
    stepFn

mkEval :: (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Term ki ty pt tm a
mkEval stepFn =
  let
    go tm = case stepFn tm of
      Just tm' -> go tm'
      Nothing -> tm
  in
    go

data MatchRule ki ty pt tm a =
    MatchBase (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a])
  | MatchEval ((Term ki ty pt tm a -> Term ki ty pt tm a) -> Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a])
  | MatchRecurse ((Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]) -> Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a])

fixMatchRule :: (Term ki ty pt tm a -> Term ki ty pt tm a) -> (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]) -> MatchRule ki ty pt tm a -> Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
fixMatchRule _ _ (MatchBase f) = f
fixMatchRule evalFn _ (MatchEval f) = f evalFn
fixMatchRule _ pMatchFn (MatchRecurse f) = f pMatchFn

mkMatch :: (Term ki ty pt tm a -> Term ki ty pt tm a) -> [MatchRule ki ty pt tm a] -> Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
mkMatch innerEval rules x y =
  let
    go p tm =
      asum .
      fmap (\r -> fixMatchRule innerEval go r p tm) $
      rules
  in
    go x y

data EvalInput ki ty pt tm a =
  EvalInput {
    eiValueRules :: [ValueRule ki ty pt tm a]
  , eiEvalRules :: [EvalRule ki ty pt tm a]
  , eiMatchRules :: [MatchRule ki ty pt tm a]
  }

instance Monoid (EvalInput ki ty pt tm a) where
  mempty =
    EvalInput mempty mempty mempty
  mappend (EvalInput v1 e1 m1) (EvalInput v2 e2 m2) =
    EvalInput
      (mappend v1 v2)
      (mappend e1 e2)
      (mappend m1 m2)

data EvalOutput ki ty pt tm a =
  EvalOutput {
    eoValue :: Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
  , eoStep :: Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
  , eoEval :: Term ki ty pt tm a -> Term ki ty pt tm a
  }

type EvalContext (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: (* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (() :: Constraint)

prepareEval :: EvalContext ki ty pt tm a
            => (Term ki ty pt tm a -> Term ki ty pt tm a)
            -> EvalInput ki ty pt tm a
            -> EvalOutput ki ty pt tm a
prepareEval innerMatchEval ei =
  let
    v = mkValue . eiValueRules $ ei
    s = mkStep v m . eiEvalRules $ ei
    e = mkEval s
    m = mkMatch innerMatchEval . eiMatchRules $ ei
  in
    EvalOutput v s e

prepareEvalStrict :: EvalContext ki ty pt tm a
                  => EvalInput ki ty pt tm a
                  -> EvalOutput ki ty pt tm a
prepareEvalStrict = prepareEval id

prepareEvalLazy :: EvalContext ki ty pt tm a
                => EvalInput ki ty pt tm a
                -> EvalOutput ki ty pt tm a
prepareEvalLazy ei =
  let
    fo = prepareEval e ei
    e = eoEval fo
  in
    fo
