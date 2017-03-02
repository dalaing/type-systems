{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.HM.Rules.Term (
    HMTermContext
  , hmTermRules
  ) where

import Bound (instantiate1)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import Rules.Term
import Ast.Term

import Fragment.HM.Ast.Term

valTmLam :: AsTmHM ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
valTmLam tm = do
  _ <- preview _TmLam tm
  return  tm

stepTmApp1 :: AsTmHM ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmApp1 evalFn tm = do
  (f, x) <- preview _TmApp tm
  f' <- evalFn f
  return $ review _TmApp (f', x)

stepTmLamAppLazy :: (AstBound ki ty pt tm, AsTmHM ki ty pt tm) => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmLamAppLazy tm = do
  (tmF, tmX) <- preview _TmApp tm
  s <- preview _TmLam tmF
  return . review _Wrapped . instantiate1 (review _Unwrapped tmX) $ s

stepTmLamAppStrict :: (AstBound ki ty pt tm, AsTmHM ki ty pt tm) => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmLamAppStrict valueFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  s <- preview _TmLam tmF
  vX <- valueFn tmX
  return . review _Wrapped . instantiate1 (review _Unwrapped vX) $ s

stepTmApp2 :: AsTmHM ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmApp2 valueFn stepFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  vF <- valueFn tmF
  tmX' <- stepFn tmX
  return $ review _TmApp (vF, tmX')

type HMTermContext ki ty pt tm a = (TermContext ki ty pt tm a, AsTmHM ki ty pt tm)

hmEvalRulesStrict :: HMTermContext ki ty pt tm a
                    => EvalInput ki ty pt tm a
hmEvalRulesStrict =
  EvalInput
  [ ValueBase valTmLam ]
  [ EvalStep stepTmApp1
  , EvalValue stepTmLamAppStrict
  , EvalValueStep stepTmApp2
  ]
  []

hmEvalRulesLazy :: HMTermContext ki ty pt tm a
                  => EvalInput ki ty pt tm a
hmEvalRulesLazy =
  EvalInput
  [ ValueBase valTmLam ]
  [ EvalStep stepTmApp1
  , EvalBase stepTmLamAppLazy
  ]
  []

hmTermRules :: HMTermContext ki ty pt tm a
            => TermInput ki ty pt tm a
hmTermRules =
  TermInput hmEvalRulesStrict hmEvalRulesLazy
