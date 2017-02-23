{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Bool.Rules.Eval (
    BoolEvalContext
  , boolEvalRules
  ) where

import Control.Monad (MonadPlus(..))

import Control.Lens (review, preview)

import Rules.Eval
import Ast.Pattern
import Ast.Term

import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

valBool :: AsTmBool ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
valBool tm = do
  _ <- preview _TmBool tm
  return tm

stepAnd1 :: AsTmBool ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepAnd1 stepFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  tm1' <- stepFn tm1
  return . review _TmAnd $ (tm1', tm2)

stepAnd2 :: AsTmBool ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepAnd2 stepFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  _ <- preview _TmBool tm1
  tm2' <- stepFn tm2
  return . review _TmAnd $ (tm1, tm2')

stepAndBool :: AsTmBool ty pt tm
           => Term ty pt tm a
           -> Maybe (Term ty pt tm a)
stepAndBool tm = do
  (tm1, tm2) <- preview _TmAnd tm
  b1 <- preview _TmBool tm1
  b2 <- preview _TmBool tm2
  return . review _TmBool $ b1 && b2

stepOr1 :: AsTmBool ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepOr1 stepFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  tm1' <- stepFn tm1
  return . review _TmOr $ (tm1', tm2)

stepOr2 :: AsTmBool ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepOr2 stepFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  _ <- preview _TmBool tm1
  tm2' <- stepFn tm2
  return . review _TmOr $ (tm1, tm2')

stepOrBool :: AsTmBool ty pt tm
           => Term ty pt tm a
           -> Maybe (Term ty pt tm a)
stepOrBool tm = do
  (tm1, tm2) <- preview _TmOr tm
  b1 <- preview _TmBool tm1
  b2 <- preview _TmBool tm2
  return . review _TmBool $ b1 || b2

matchBool :: (AsPtBool pt, AsTmBool ty pt tm)
          => (Term ty pt tm a -> Term ty pt tm a)
          -> Pattern pt a
          -> Term ty pt tm a
          -> Maybe [Term ty pt tm a]
matchBool eval p tm = do
  b <- preview _PtBool p
  c <- preview _TmBool (eval tm)
  if b == c
  then return []
  else mzero

type BoolEvalContext ty pt tm a = (EvalContext ty pt tm a, AsPtBool pt, AsTmBool ty pt tm)

boolEvalRules :: BoolEvalContext ty pt tm a
               => EvalInput ty pt tm a
boolEvalRules =
  EvalInput
    [ValueBase valBool]
    [ EvalStep stepAnd1
    , EvalStep stepAnd2
    , EvalBase stepAndBool
    , EvalStep stepOr1
    , EvalStep stepOr2
    , EvalBase stepOrBool
    ]
    [ MatchEval matchBool ]