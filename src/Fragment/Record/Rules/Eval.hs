{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Record.Rules.Eval (
    RecordEvalContext
  , recordEvalRulesLazy
  , recordEvalRulesStrict
  ) where

import Data.Foldable (asum)

import Control.Lens (review, preview)

import Rules.Eval
import Ast.Pattern
import Ast.Term

import Fragment.Record.Ast.Pattern
import Fragment.Record.Ast.Term

stepRecordIxLazy :: AsTmRecord ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepRecordIxLazy tm = do
  (tmT, t) <- preview _TmRecordIx tm
  tms <- preview _TmRecord tmT
  let Just tm' = lookup t tms
  return tm'

valueRecord :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
valueRecord valueFn tm = do
  tms <- preview _TmRecord tm
  vs <- traverse (traverse valueFn) tms
  return $ review _TmRecord vs

stepRecordIxStrict :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepRecordIxStrict stepFn tm = do
  (tmR, t) <- preview _TmRecordIx tm
  tmR' <- stepFn tmR
  return $ review _TmRecordIx (tmR', t)

stepRecordElimIxStrict :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepRecordElimIxStrict valueFn tm = do
  (tmR, t) <- preview _TmRecordIx tm
  tms <- preview _TmRecord tmR
  vs <- traverse (traverse valueFn) tms
  let Just v = lookup t vs
  return v

stepRecordIx :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Int -> Maybe (Term ty pt tm a)
stepRecordIx valueFn stepFn tm i = do
  tms <- preview _TmRecord tm
  let (vs, s : ts) = splitAt i tms
  vs' <- traverse (traverse valueFn) vs
  s' <- traverse stepFn s
  return $ review _TmRecord (vs' ++ s' : ts)

stepRecord :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepRecord valueFn stepFn tm = do
  tms <- preview _TmRecord tm
  let l = length tms
  asum . fmap (stepRecordIx valueFn stepFn tm) $ [0..l-1]

matchRecord :: (AsPtRecord pt, AsTmRecord ty pt tm) => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
matchRecord matchFn p tm = do
  ps <- preview _PtRecord p
  ltms <- preview _TmRecord tm
  let f (l, lp) = do
        tmp <- lookup l ltms
        matchFn lp tmp
  fmap mconcat . traverse f $ ps

type RecordEvalContext ty pt tm a = (EvalContext ty pt tm a, AsPtRecord pt, AsTmRecord ty pt tm)

-- TODO check this, there might be more rules
recordEvalRulesLazy :: RecordEvalContext ty pt tm a
                   => EvalInput ty pt tm a
recordEvalRulesLazy =
  EvalInput
  [] [EvalBase stepRecordIxLazy] [ MatchRecurse matchRecord ]

recordEvalRulesStrict :: RecordEvalContext ty pt tm a
                     => EvalInput ty pt tm a
recordEvalRulesStrict =
  EvalInput
    [ ValueRecurse valueRecord ]
    [ EvalStep stepRecordIxStrict
    , EvalValue stepRecordElimIxStrict
    , EvalValueStep stepRecord
    ]
    [ MatchRecurse matchRecord ]
