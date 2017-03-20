{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Tuple.Rules.Term (
    TupleEvalConstraint
  , tupleEvalRulesStrict
  , tupleEvalRulesLazy
  ) where

import Control.Monad (zipWithM)
import Data.Foldable (asum)

import Control.Lens (review, preview)

import Rules.Term
import Ast.Pattern
import Ast.Term

import Fragment.Tuple.Ast.Pattern
import Fragment.Tuple.Ast.Term

stepTupleIxLazy :: AsTmTuple ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTupleIxLazy tm = do
  (tmT ,i) <- preview _TmTupleIx tm
  tms <- preview _TmTuple tmT
  return $ tms !! i

valueTuple :: AsTmTuple ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
valueTuple valueFn tm = do
  tms <- preview _TmTuple tm
  vs <- traverse valueFn tms
  return $ review _TmTuple vs

stepTupleIxStrict :: AsTmTuple ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTupleIxStrict stepFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  tmT' <- stepFn tmT
  return $ review _TmTupleIx (tmT', i)

stepTupleElimIxStrict :: AsTmTuple ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTupleElimIxStrict valueFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  tms <- preview _TmTuple tmT
  vs <- traverse valueFn tms
  return $ vs !! i

stepTupleIx :: AsTmTuple ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Int -> Maybe (Term ki ty pt tm a)
stepTupleIx valueFn stepFn tm i = do
  tms <- preview _TmTuple tm
  let (vs, s : ts) = splitAt i tms
  vs' <- traverse valueFn vs
  s' <- stepFn s
  return $ review _TmTuple (vs' ++ s' : ts)

stepTuple :: AsTmTuple ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTuple valueFn stepFn tm = do
  tms <- preview _TmTuple tm
  let l = length tms
  asum . fmap (stepTupleIx valueFn stepFn tm) $ [0..l-1]

matchTuple :: (AsPtTuple pt, AsTmTuple ki ty pt tm) => (Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]) -> Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
matchTuple matchFn p tm = do
  pts <- preview _PtTuple p
  tms <- preview _TmTuple tm
  tmss <- zipWithM matchFn pts tms
  return $ mconcat tmss

type TupleEvalConstraint ki ty pt tm a =
  ( AsPtTuple pt
  , AsTmTuple ki ty pt tm
  )

tupleEvalRulesStrict :: TupleEvalConstraint ki ty pt tm a
                     => EvalInput ki ty pt tm a
tupleEvalRulesStrict =
  EvalInput
    [ ValueRecurse valueTuple ]
    [ EvalStep stepTupleIxStrict
    , EvalValue stepTupleElimIxStrict
    , EvalValueStep stepTuple
    ]
    [ MatchRecurse matchTuple ]

tupleEvalRulesLazy :: TupleEvalConstraint ki ty pt tm a
                   => EvalInput ki ty pt tm a
tupleEvalRulesLazy =
  EvalInput
  [] [EvalBase stepTupleIxLazy] [ MatchRecurse matchTuple ]
