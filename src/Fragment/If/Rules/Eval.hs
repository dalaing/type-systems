{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.If.Rules.Eval (
    IfEvalContext
  , ifEvalRules
  ) where

import Control.Lens (review, preview)

import Rules.Eval
import Ast.Term

import Fragment.Bool.Ast.Term
import Fragment.If.Ast.Term

stepIf1 :: AsTmIf ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepIf1 stepFn tm = do
  (tmB, tmT, tmE) <- preview _TmIf tm
  tmB' <- stepFn tmB
  return $ review _TmIf (tmB', tmT, tmE)

stepIf2 :: (AsTmIf ty pt tm, AsTmBool ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepIf2 valueFn tm = do
  (tmB, tmT, tmF) <- preview _TmIf tm
  vB <- valueFn tmB
  b <- preview _TmBool vB
  return $
    if b then tmT else tmF

type IfEvalContext ty pt tm a = (EvalContext ty pt tm a, AsTmBool ty pt tm, AsTmIf ty pt tm)

ifEvalRules :: IfEvalContext ty pt tm a
            => EvalInput ty pt tm a
ifEvalRules =
  EvalInput [] [ EvalStep stepIf1, EvalValue stepIf2] []
