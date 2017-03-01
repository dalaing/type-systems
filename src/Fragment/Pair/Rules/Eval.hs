{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Pair.Rules.Eval (
    PairEvalContext
  , pairEvalRulesLazy
  , pairEvalRulesStrict
  ) where

import Control.Lens (review, preview)

import Rules.Eval
import Ast.Pattern
import Ast.Term

import Fragment.Pair.Ast.Pattern
import Fragment.Pair.Ast.Term

stepFstLazy :: AsTmPair ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepFstLazy tm = do
  tmP <- preview _TmFst tm
  (tm1, _) <- preview _TmPair tmP
  return tm1

stepSndLazy :: AsTmPair ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepSndLazy tm = do
  tmP <- preview _TmSnd tm
  (_, tm2) <- preview _TmPair tmP
  return tm2

valuePair :: AsTmPair ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
valuePair valueFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  v1 <- valueFn tm1
  v2 <- valueFn tm2
  return $ review _TmPair (v1, v2)

stepFstStrict :: AsTmPair ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepFstStrict stepFn tm = do
  tmP <- preview _TmFst tm
  tmP' <- stepFn tmP
  return $ review _TmFst tmP'

stepSndStrict :: AsTmPair ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepSndStrict stepFn tm = do
  tmP <- preview _TmSnd tm
  tmP' <- stepFn tmP
  return $ review _TmSnd tmP'

stepElimFstStrict :: AsTmPair ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepElimFstStrict valueFn tm = do
  tmP <- preview _TmFst tm
  (tm1, tm2) <- preview _TmPair tmP
  v1 <- valueFn tm1
  _ <- valueFn tm2
  return v1

stepElimSndStrict :: AsTmPair ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepElimSndStrict valueFn tm = do
  tmP <- preview _TmSnd tm
  (tm1, tm2) <- preview _TmPair tmP
  _ <- valueFn tm1
  v2 <- valueFn tm2
  return v2

stepPair1 :: AsTmPair ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepPair1 stepFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  tm1' <- stepFn tm1
  return $ review _TmPair (tm1', tm2)

stepPair2 :: AsTmPair ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepPair2 valueFn stepFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  v1 <- valueFn tm1
  tm2' <- stepFn tm2
  return $ review _TmPair (v1, tm2')

matchPair :: (AsPtPair pt, AsTmPair ki ty pt tm) => (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]) -> Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
matchPair matchFn p tm = do
  (p1, p2) <- preview _PtPair p
  (tm1, tm2) <- preview _TmPair tm
  tms1 <- matchFn p1 tm1
  tms2 <- matchFn p2 tm2
  return $ tms1 ++ tms2

type PairEvalContext ki ty pt tm a = (EvalContext ki ty pt tm a, AsPtPair pt, AsTmPair ki ty pt tm)

pairEvalRulesLazy :: PairEvalContext ki ty pt tm a
                  => EvalInput ki ty pt tm a
pairEvalRulesLazy =
  EvalInput
    []
    [ EvalBase stepFstLazy
    , EvalBase stepSndLazy
    ]
    [ MatchRecurse matchPair ]

pairEvalRulesStrict :: PairEvalContext ki ty pt tm a
                    => EvalInput ki ty pt tm a
pairEvalRulesStrict =
  EvalInput
    [ ValueRecurse valuePair ]
    [ EvalStep stepFstStrict
    , EvalStep stepSndStrict
    , EvalValue stepElimFstStrict
    , EvalValue stepElimSndStrict
    , EvalStep stepPair1
    , EvalValueStep stepPair2
    ]
    [ MatchRecurse matchPair ]

