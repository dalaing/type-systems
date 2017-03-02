{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Int.Rules.Term (
    IntTermContext
  , intTermRules
  ) where

import Control.Monad (MonadPlus(..))

import Control.Lens (review, preview)

import Rules.Term
import Ast.Pattern
import Ast.Term

import Fragment.Int.Ast.Pattern
import Fragment.Int.Ast.Term

valInt :: AsTmInt ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
valInt tm = do
  _ <- preview _TmInt tm
  return tm

stepAdd1 :: AsTmInt ki ty pt tm
         => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
         -> Term ki ty pt tm a
         -> Maybe (Term ki ty pt tm a)
stepAdd1 stepFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  tm1' <- stepFn tm1
  return . review _TmAdd $ (tm1', tm2)

stepAdd2 :: AsTmInt ki ty pt tm
         => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
         -> Term ki ty pt tm a
         -> Maybe (Term ki ty pt tm a)
stepAdd2 stepFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  _ <- preview _TmInt tm1
  tm2' <- stepFn tm2
  return . review _TmAdd $ (tm1, tm2')

stepAddInt :: AsTmInt ki ty pt tm
           => Term ki ty pt tm a
           -> Maybe (Term ki ty pt tm a)
stepAddInt tm = do
  (tm1, tm2) <- preview _TmAdd tm
  i1 <- preview _TmInt tm1
  i2 <- preview _TmInt tm2
  return . review _TmInt $ i1 + i2

stepMul1 :: AsTmInt ki ty pt tm
         => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
         -> Term ki ty pt tm a
         -> Maybe (Term ki ty pt tm a)
stepMul1 stepFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  tm1' <- stepFn tm1
  return . review _TmMul $ (tm1', tm2)

stepMul2 :: AsTmInt ki ty pt tm
         => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
         -> Term ki ty pt tm a
         -> Maybe (Term ki ty pt tm a)
stepMul2 stepFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  _ <- preview _TmInt tm1
  tm2' <- stepFn tm2
  return . review _TmMul $ (tm1, tm2')

stepMulInt :: AsTmInt ki ty pt tm
           => Term ki ty pt tm a
           -> Maybe (Term ki ty pt tm a)
stepMulInt tm = do
  (tm1, tm2) <- preview _TmMul tm
  i1 <- preview _TmInt tm1
  i2 <- preview _TmInt tm2
  return . review _TmInt $ i1 * i2

matchInt :: (AsPtInt pt, AsTmInt ki ty pt tm)
         => (Term ki ty pt tm a -> Term ki ty pt tm a)
         -> Pattern pt a
         -> Term ki ty pt tm a
         -> Maybe [Term ki ty pt tm a]
matchInt eval p tm = do
  i <- preview _PtInt p
  j <- preview _TmInt (eval tm)
  if i == j
  then return []
  else mzero

type IntTermContext ki ty pt tm a = (TermContext ki ty pt tm a, AsPtInt pt, AsTmInt ki ty pt tm)

intEvalRules :: IntTermContext ki ty pt tm a
             => EvalInput ki ty pt tm a
intEvalRules =
  EvalInput
    [ValueBase valInt]
    [ EvalStep stepAdd1
    , EvalStep stepAdd2
    , EvalBase stepAddInt
    , EvalStep stepMul1
    , EvalStep stepMul2
    , EvalBase stepMulInt
    ]
    [ MatchEval matchInt ]

intTermRules :: IntTermContext ki ty pt tm a
             => TermInput ki ty pt tm a
intTermRules =
  TermInput intEvalRules intEvalRules
