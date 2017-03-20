{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.IsoRec.Rules.Term (
    IsoRecEvalConstraint
  , isoRecEvalRulesStrict
  , isoRecEvalRulesLazy
  ) where

import Control.Lens (review, preview)

import Rules.Term
import Ast.Term

import Fragment.IsoRec.Ast.Term

valTmFoldStrict :: AsTmIsoRec ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
valTmFoldStrict valueFn tm = do
  (tyF, tmF) <- preview _TmFold tm
  vF <- valueFn tmF
  return $ review _TmFold (tyF, vF)

valTmFoldLazy :: AsTmIsoRec ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
valTmFoldLazy tm = do
  _ <- preview _TmFold tm
  return tm

stepTmFold :: AsTmIsoRec ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmFold evalFn tm = do
  (tyF, tmF) <- preview _TmFold tm
  tmF' <- evalFn tmF
  return $ review _TmFold (tyF, tmF')

stepTmUnfold :: AsTmIsoRec ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmUnfold evalFn tm = do
  (tyF, tmF) <- preview _TmUnfold tm
  tmF' <- evalFn tmF
  return $ review _TmUnfold (tyF, tmF')

stepTmUnfoldFoldStrict :: (AstBound ki ty pt tm, AsTmIsoRec ki ty pt tm) => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmUnfoldFoldStrict valueFn tm = do
  (_, tmU) <- preview _TmUnfold tm
  (_, tmF) <- preview _TmFold tmU
  vF <- valueFn tmF
  return vF

stepTmUnfoldFoldLazy :: (AstBound ki ty pt tm, AsTmIsoRec ki ty pt tm) => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmUnfoldFoldLazy tm = do
  (_, tmU) <- preview _TmUnfold tm
  (_, tmF) <- preview _TmFold tmU
  return tmF

type IsoRecEvalConstraint ki ty pt tm a =
  AsTmIsoRec ki ty pt tm

isoRecEvalRulesStrict :: IsoRecEvalConstraint ki ty pt tm a
                      => EvalInput ki ty pt tm a
isoRecEvalRulesStrict =
  EvalInput
  [ ValueRecurse valTmFoldStrict ]
  [ StepRecurse stepTmUnfold
  , StepRecurse stepTmFold
  , StepValue stepTmUnfoldFoldStrict
  ]
  []

isoRecEvalRulesLazy :: IsoRecEvalConstraint ki ty pt tm a
                    => EvalInput ki ty pt tm a
isoRecEvalRulesLazy =
  EvalInput
  [ ValueBase valTmFoldLazy ]
  [ StepRecurse stepTmUnfold
  , StepBase stepTmUnfoldFoldLazy
  ]
  []
