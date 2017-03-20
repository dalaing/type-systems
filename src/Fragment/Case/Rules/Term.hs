{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module Fragment.Case.Rules.Term (
    CaseEvalConstraint
  , caseEvalRulesLazy
  , caseEvalRulesStrict
  ) where

import Bound (instantiate)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import qualified Data.List.NonEmpty as N

import Rules.Term
import Ast.Pattern
import Ast.Term

import Fragment.Case.Ast.Term

type MatchConstraint (ki :: * -> *) ty pt tm = (AstTransversable ki ty pt tm, AstBound ki ty pt tm)

handleMatch :: MatchConstraint ki ty pt tm
            => (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a])
            -> Term ki ty pt tm a -> N.NonEmpty (Alt ki ty pt (Ast ki ty pt tm ) (AstVar a))
            -> Maybe (Term ki ty pt tm a)
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

stepCaseLazy :: (MatchConstraint ki ty pt tm, AsTmCase ki ty pt tm) => (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepCaseLazy matchFn tm = do
  (tmC, alts) <- preview _TmCase tm
  handleMatch matchFn tmC alts

stepCaseStepStrict :: AsTmCase ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepCaseStepStrict stepFn tm = do
  (tmC, alts) <- preview _TmCase tm
  tmC' <- stepFn tmC
  return $ review _TmCase (tmC', alts)

stepCaseValueStrict :: (MatchConstraint ki ty pt tm, AsTmCase ki ty pt tm) => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepCaseValueStrict valueFn matchFn tm = do
  (tmC, alts) <- preview _TmCase tm
  vC <- valueFn tmC
  handleMatch matchFn vC alts

type CaseEvalConstraint ki ty pt tm a =
  ( AstBound ki ty pt tm
  , AstTransversable ki ty pt tm
  , AsTmCase ki ty pt tm
  )

caseEvalRulesStrict :: CaseEvalConstraint ki ty pt tm a
                    => EvalInput ki ty pt tm a
caseEvalRulesStrict =
  EvalInput
    []
    [ StepRecurse stepCaseStepStrict
    , StepValueMatch stepCaseValueStrict
    ]
    []

caseEvalRulesLazy :: CaseEvalConstraint ki ty pt tm a
                  => EvalInput ki ty pt tm a
caseEvalRulesLazy =
  EvalInput [] [ StepMatch stepCaseLazy ] []
