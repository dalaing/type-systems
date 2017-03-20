{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.TmApp.Rules.Term (
    TmAppEvalConstraint
  , tmAppEvalRulesStrict
  , tmAppEvalRulesLazy
  ) where

import Control.Lens (review, preview)

import Rules.Term
import Ast.Term

import Fragment.TmApp.Ast.Term

stepTmApp1 :: AsTmApp ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmApp1 evalFn tm = do
  (f, x) <- preview _TmApp tm
  f' <- evalFn f
  return $ review _TmApp (f', x)

stepTmApp2 :: AsTmApp ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmApp2 valueFn stepFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  vF <- valueFn tmF
  tmX' <- stepFn tmX
  return $ review _TmApp (vF, tmX')

type TmAppEvalConstraint ki ty pt tm a =
  AsTmApp ki ty pt tm

tmAppEvalRulesStrict :: TmAppEvalConstraint ki ty pt tm a
                     => EvalInput ki ty pt tm a
tmAppEvalRulesStrict =
  EvalInput
    []
    [ EvalStep stepTmApp1
    , EvalValueStep stepTmApp2
    ]
    []

tmAppEvalRulesLazy :: TmAppEvalConstraint ki ty pt tm a
                  => EvalInput ki ty pt tm a
tmAppEvalRulesLazy =
  EvalInput
    []
    [ EvalStep stepTmApp1 ]
    []

