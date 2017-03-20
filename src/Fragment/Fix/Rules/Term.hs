{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Fix.Rules.Term (
    FixEvalConstraint
  , fixEvalRules
  ) where

import Bound (instantiate1)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import Ast.Term
import Rules.Term

import Fragment.TmLam.Ast.Term
import Fragment.Fix.Ast.Term

stepTmFix1 :: (AsTmFix ki ty pt tm)
           => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
           -> Term ki ty pt tm a
           -> Maybe (Term ki ty pt tm a)
stepTmFix1 stepFn tm = do
  tmF <- preview _TmFix tm
  tmF' <- stepFn tmF
  return $ review _TmFix tmF'

stepTmFixBeta :: (AsTmFix ki ty pt tm, AsTmLam ki ty pt tm)
              => Term ki ty pt tm a
              -> Maybe (Term ki ty pt tm a)
stepTmFixBeta tm = do
  tmF <- preview _TmFix tm
  (_, s) <- preview _TmLam tmF
  return . review _Wrapped . instantiate1 (review _Unwrapped tm) $ s

type FixEvalConstraint ki ty pt tm a =
  ( AsTmFix ki ty pt tm
  , AsTmLam ki ty pt tm
  )

fixEvalRules :: FixEvalConstraint ki ty pt tm a
             => EvalInput ki ty pt tm a
fixEvalRules =
  EvalInput
    []
    [ EvalStep stepTmFix1
    , EvalBase stepTmFixBeta
    ]
    []
