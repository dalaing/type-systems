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
module Fragment.STLC.Rules (
    RSTLC
  ) where

import Rules

import Fragment.STLC.Ast
-- import Fragment.STLC.Rules.Infer
import Fragment.STLC.Rules.Infer.Unification.Offline
import Fragment.STLC.Rules.Eval

data RSTLC

instance RulesIn RSTLC where
  type RuleInferContext e w s r m ty pt tm a RSTLC = STLCInferContext e w s r m ty pt tm a
  type RuleEvalContext ty pt tm a RSTLC = STLCEvalContext ty pt tm a
  type TypeList RSTLC = '[TyFSTLC]
  type ErrorList ty pt tm a RSTLC = '[ErrExpectedTyArr ty a]
  type WarningList ty pt tm a RSTLC = '[]
  type PatternList RSTLC = '[]
  type TermList RSTLC = '[TmFSTLC]

  inferInput _ = stlcInferRules
  evalLazyInput _ = stlcEvalRulesLazy
  evalStrictInput _ = stlcEvalRulesStrict
