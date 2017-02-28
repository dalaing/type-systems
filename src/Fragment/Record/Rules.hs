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
module Fragment.Record.Rules (
    RRecord
  ) where

import Rules

import Fragment.Record.Ast
-- import Fragment.Record.Rules.Infer
import Fragment.Record.Rules.Infer.Unification.Offline
import Fragment.Record.Rules.Eval

data RRecord

instance RulesIn RRecord where
  type RuleInferContext e w s r m ty pt tm a RRecord = RecordInferContext e w s r m ty pt tm a
  type RuleEvalContext ty pt tm a RRecord = RecordEvalContext ty pt tm a
  type TypeList RRecord = '[TyFRecord]
  type ErrorList ty pt tm a RRecord = '[ErrExpectedTyRecord ty a, ErrRecordNotFound]
  type WarningList ty pt tm a RRecord = '[]
  type PatternList RRecord = '[PtFRecord]
  type TermList RRecord = '[TmFRecord]

  inferInput _ = recordInferRules
  evalLazyInput _ = recordEvalRulesLazy
  evalStrictInput _ = recordEvalRulesStrict
