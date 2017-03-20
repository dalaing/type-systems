{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.LC.Rules.Term (
    LCEvalConstraint
  , lcEvalRulesStrict
  , lcEvalRulesLazy
  ) where

import Bound (instantiate1)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import Rules.Term
import Ast.Term

import Fragment.TmLam.Ast.Term
import Fragment.TmApp.Ast.Term

stepTmLamAppLazy :: (AstBound ki ty pt tm, AsTmLam ki ty pt tm, AsTmApp ki ty pt tm) => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmLamAppLazy tm = do
  (tmF, tmX) <- preview _TmApp tm
  (_, s) <- preview _TmLam tmF
  return . review _Wrapped . instantiate1 (review _Unwrapped tmX) $ s

stepTmLamAppStrict :: (AstBound ki ty pt tm, AsTmLam ki ty pt tm, AsTmApp ki ty pt tm) => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmLamAppStrict valueFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  (_, s) <- preview _TmLam tmF
  vX <- valueFn tmX
  return . review _Wrapped . instantiate1 (review _Unwrapped vX) $ s

type LCEvalConstraint ki ty pt tm a =
  ( AsTmLam ki ty pt tm
  , AsTmApp ki ty pt tm
  )

lcEvalRulesStrict :: LCEvalConstraint ki ty pt tm a
                  => EvalInput ki ty pt tm a
lcEvalRulesStrict =
  EvalInput [] [ StepValue stepTmLamAppStrict ] []

lcEvalRulesLazy :: LCEvalConstraint ki ty pt tm a
                => EvalInput ki ty pt tm a
lcEvalRulesLazy =
  EvalInput [] [ StepBase stepTmLamAppLazy ] []
