{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.STLC.Rules.Eval (
    STLCEvalContext
  , stlcEvalRulesLazy
  , stlcEvalRulesStrict
  ) where

import Bound (instantiate1)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import Rules.Eval
import Ast.Term

import Fragment.STLC.Ast.Term

valTmLam :: AsTmSTLC ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
valTmLam tm = do
  _ <- preview _TmLam tm
  return  tm

stepTmApp1 :: AsTmSTLC ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmApp1 evalFn tm = do
  (f, x) <- preview _TmApp tm
  f' <- evalFn f
  return $ review _TmApp (f', x)

stepTmLamAppLazy :: (AstBound ty pt tm, AsTmSTLC ty pt tm) => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmLamAppLazy tm = do
  (tmF, tmX) <- preview _TmApp tm
  (_, s) <- preview _TmLam tmF
  return . review _Wrapped . instantiate1 (review _Unwrapped tmX) $ s

stepTmLamAppStrict :: (AstBound ty pt tm, AsTmSTLC ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmLamAppStrict valueFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  (_, s) <- preview _TmLam tmF
  vX <- valueFn tmX
  return . review _Wrapped . instantiate1 (review _Unwrapped vX) $ s

stepTmApp2 :: AsTmSTLC ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmApp2 valueFn stepFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  vF <- valueFn tmF
  tmX' <- stepFn tmX
  return $ review _TmApp (vF, tmX')

type STLCEvalContext ty pt tm a = (EvalContext ty pt tm a, AsTmSTLC ty pt tm)

stlcEvalRulesLazy :: STLCEvalContext ty pt tm a
                  => EvalInput ty pt tm a
stlcEvalRulesLazy =
  EvalInput
  [ ValueBase valTmLam ]
  [ EvalStep stepTmApp1
  , EvalBase stepTmLamAppLazy
  ]
  []

stlcEvalRulesStrict :: STLCEvalContext ty pt tm a
                    => EvalInput ty pt tm a
stlcEvalRulesStrict =
  EvalInput
  [ ValueBase valTmLam ]
  [ EvalStep stepTmApp1
  , EvalValue stepTmLamAppStrict
  , EvalValueStep stepTmApp2
  ]
  []

