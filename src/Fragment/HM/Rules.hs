{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.HM.Rules (
    RHM
  ) where

import Rules

import Fragment.HM.Ast
-- import Fragment.HM.Rules.Infer
import Fragment.HM.Rules.Infer.Unification.Offline
import Fragment.HM.Rules.Eval

data RHM

instance RulesIn RHM where
  type RuleInferContext e w s r m ty pt tm a RHM = HMInferContext e w s r m ty pt tm a
  type RuleEvalContext ty pt tm a RHM = HMEvalContext ty pt tm a
  type TypeList RHM = '[TyFHM]
  type ErrorList ty pt tm a RHM = '[ErrExpectedTyArr ty a]
  type WarningList ty pt tm a RHM = '[]
  type PatternList RHM = '[]
  type TermList RHM = '[TmFHM]

  inferInput _ = hmInferRules
  evalLazyInput _ = hmEvalRulesLazy
  evalStrictInput _ = hmEvalRulesStrict
