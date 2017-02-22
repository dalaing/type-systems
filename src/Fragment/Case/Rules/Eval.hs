{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Case.Rules.Eval (
    CaseEvalContext
  , caseEvalRulesLazy
  , caseEvalRulesStrict
  ) where

import Bound (instantiate)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import qualified Data.List.NonEmpty as N

import Rules.Eval
import Ast.Pattern
import Ast.Term

import Fragment.Case.Ast.Term

type MatchConstraint ty pt tm = (AstTransversable ty pt tm, AstBound ty pt tm)

handleMatch :: MatchConstraint ty pt tm
            => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])
            -> Term ty pt tm a -> N.NonEmpty (Alt ty pt (Ast ty pt tm ) (AstVar a))
            -> Maybe (Term ty pt tm a)
handleMatch matchFn tm alts =
  let
    go (Alt p s : rest) = do
      p' <- preview _Pattern p
      case matchFn p' tm of
        Nothing -> go rest
        Just tms -> return . review _Wrapped . instantiate (review _Unwrapped . (tms !!)) $ s
    go [] = Nothing
  in
    go (N.toList alts)

stepCaseLazy :: (MatchConstraint ty pt tm, AsTmCase ty pt tm) => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepCaseLazy matchFn tm = do
  (tmC, alts) <- preview _TmCase tm
  handleMatch matchFn tmC alts

stepCaseStepStrict :: AsTmCase ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepCaseStepStrict stepFn tm = do
  (tmC, alts) <- preview _TmCase tm
  tmC' <- stepFn tmC
  return $ review _TmCase (tmC', alts)

stepCaseValueStrict :: (MatchConstraint ty pt tm, AsTmCase ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepCaseValueStrict valueFn matchFn tm = do
  (tmC, alts) <- preview _TmCase tm
  vC <- valueFn tmC
  handleMatch matchFn vC alts

type CaseEvalContext ty pt tm a = (EvalContext ty pt tm a, AstBound ty pt tm, AstTransversable ty pt tm, AsTmCase ty pt tm)

-- TODO check this, there might be more rules
caseEvalRulesLazy :: CaseEvalContext ty pt tm a
                  => EvalInput ty pt tm a
caseEvalRulesLazy =
  EvalInput
    []
    [EvalMatch stepCaseLazy]
    []

caseEvalRulesStrict :: CaseEvalContext ty pt tm a
                    => EvalInput ty pt tm a
caseEvalRulesStrict =
  EvalInput
    []
    [ EvalStep stepCaseStepStrict
    , EvalValueMatch stepCaseValueStrict
    ]
    []
