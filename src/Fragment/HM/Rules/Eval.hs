{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.HM.Rules.Eval (
    HMEvalContext
  , hmEvalRulesLazy
  , hmEvalRulesStrict
  ) where

import Bound (instantiate1)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import Rules.Eval
import Ast.Term

import Fragment.HM.Ast.Term

valTmLam :: AsTmHM ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
valTmLam tm = do
  _ <- preview _TmLam tm
  return  tm

stepTmApp1 :: AsTmHM ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmApp1 evalFn tm = do
  (f, x) <- preview _TmApp tm
  f' <- evalFn f
  return $ review _TmApp (f', x)

stepTmLamAppLazy :: (AstBound ty pt tm, AsTmHM ty pt tm) => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmLamAppLazy tm = do
  (tmF, tmX) <- preview _TmApp tm
  s <- preview _TmLam tmF
  return . review _Wrapped . instantiate1 (review _Unwrapped tmX) $ s

stepTmLamAppStrict :: (AstBound ty pt tm, AsTmHM ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmLamAppStrict valueFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  s <- preview _TmLam tmF
  vX <- valueFn tmX
  return . review _Wrapped . instantiate1 (review _Unwrapped vX) $ s

stepTmApp2 :: AsTmHM ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmApp2 valueFn stepFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  vF <- valueFn tmF
  tmX' <- stepFn tmX
  return $ review _TmApp (vF, tmX')

type HMEvalContext ty pt tm a = (EvalContext ty pt tm a, AsTmHM ty pt tm)

hmEvalRulesLazy :: HMEvalContext ty pt tm a
                  => EvalInput ty pt tm a
hmEvalRulesLazy =
  EvalInput
  [ ValueBase valTmLam ]
  [ EvalStep stepTmApp1
  , EvalBase stepTmLamAppLazy
  ]
  []

hmEvalRulesStrict :: HMEvalContext ty pt tm a
                    => EvalInput ty pt tm a
hmEvalRulesStrict =
  EvalInput
  [ ValueBase valTmLam ]
  [ EvalStep stepTmApp1
  , EvalValue stepTmLamAppStrict
  , EvalValueStep stepTmApp2
  ]
  []

