{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Record.Rules.Term (
    RecordEvalConstraint
  , recordEvalRulesStrict
  , recordEvalRulesLazy
  ) where

import Data.Foldable (asum)

import Control.Lens (review, preview)

import Rules.Term
import Ast.Pattern
import Ast.Term

import Fragment.Record.Ast.Pattern
import Fragment.Record.Ast.Term

stepRecordIxLazy :: AsTmRecord ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepRecordIxLazy tm = do
  (tmT, t) <- preview _TmRecordIx tm
  tms <- preview _TmRecord tmT
  let Just tm' = lookup t tms
  return tm'

valueRecord :: AsTmRecord ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
valueRecord valueFn tm = do
  tms <- preview _TmRecord tm
  vs <- traverse (traverse valueFn) tms
  return $ review _TmRecord vs

stepRecordIxStrict :: AsTmRecord ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepRecordIxStrict stepFn tm = do
  (tmR, t) <- preview _TmRecordIx tm
  tmR' <- stepFn tmR
  return $ review _TmRecordIx (tmR', t)

stepRecordElimIxStrict :: AsTmRecord ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepRecordElimIxStrict valueFn tm = do
  (tmR, t) <- preview _TmRecordIx tm
  tms <- preview _TmRecord tmR
  vs <- traverse (traverse valueFn) tms
  let Just v = lookup t vs
  return v

stepRecordIx :: AsTmRecord ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Int -> Maybe (Term ki ty pt tm a)
stepRecordIx valueFn stepFn tm i = do
  tms <- preview _TmRecord tm
  let (vs, s : ts) = splitAt i tms
  vs' <- traverse (traverse valueFn) vs
  s' <- traverse stepFn s
  return $ review _TmRecord (vs' ++ s' : ts)

stepRecord :: AsTmRecord ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepRecord valueFn stepFn tm = do
  tms <- preview _TmRecord tm
  let l = length tms
  asum . fmap (stepRecordIx valueFn stepFn tm) $ [0..l-1]

matchRecord :: (AsPtRecord pt, AsTmRecord ki ty pt tm) => (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]) -> Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
matchRecord matchFn p tm = do
  ps <- preview _PtRecord p
  ltms <- preview _TmRecord tm
  let f (l, lp) = do
        tmp <- lookup l ltms
        matchFn lp tmp
  fmap mconcat . traverse f $ ps

type RecordEvalConstraint ki ty pt tm a =
  ( AsPtRecord pt
  , AsTmRecord ki ty pt tm
  )

recordEvalRulesStrict :: RecordEvalConstraint ki ty pt tm a
                      => EvalInput ki ty pt tm a
recordEvalRulesStrict =
  EvalInput
    [ ValueRecurse valueRecord ]
    [ StepRecurse stepRecordIxStrict
    , StepValue stepRecordElimIxStrict
    , StepValueRecurse stepRecord
    ]
    [ MatchRecurse matchRecord ]

recordEvalRulesLazy :: RecordEvalConstraint ki ty pt tm a
                    => EvalInput ki ty pt tm a
recordEvalRulesLazy =
  EvalInput
  [] [StepBase stepRecordIxLazy] [ MatchRecurse matchRecord ]
