{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.If.Rules.Term (
    IfEvalConstraint
  , ifEvalRules
  ) where

import Control.Lens (review, preview)

import Rules.Term
import Ast.Term

import Fragment.Bool.Ast.Term
import Fragment.If.Ast.Term

stepIf1 :: AsTmIf ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepIf1 stepFn tm = do
  (tmB, tmT, tmE) <- preview _TmIf tm
  tmB' <- stepFn tmB
  return $ review _TmIf (tmB', tmT, tmE)

stepIf2 :: (AsTmIf ki ty pt tm, AsTmBool ki ty pt tm) => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepIf2 valueFn tm = do
  (tmB, tmT, tmF) <- preview _TmIf tm
  vB <- valueFn tmB
  b <- preview _TmBool vB
  return $
    if b then tmT else tmF

type IfEvalConstraint ki ty pt tm a =
  ( AsTmBool ki ty pt tm
  , AsTmIf ki ty pt tm
  )

ifEvalRules :: IfEvalConstraint ki ty pt tm a
            => EvalInput ki ty pt tm a
ifEvalRules =
  EvalInput [] [ StepRecurse stepIf1, StepValue stepIf2] []
