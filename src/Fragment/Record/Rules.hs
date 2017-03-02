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
import qualified Fragment.Record.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.Record.Rules.Infer.Unification.Offline as UO
import Fragment.Record.Rules.Term

data RRecord

instance RulesIn RRecord where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RRecord = SD.RecordInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RRecord = UO.RecordInferContext e w s r m ki ty pt tm a
  type RuleTermContext ki ty pt tm a RRecord = RecordTermContext ki ty pt tm a
  type KindList RRecord = '[]
  type TypeList RRecord = '[TyFRecord]
  type ErrorList ki ty pt tm a RRecord = '[ErrExpectedTyRecord ki ty a, ErrRecordNotFound]
  type WarningList ki ty pt tm a RRecord = '[]
  type PatternList RRecord = '[PtFRecord]
  type TermList RRecord = '[TmFRecord]

  inferSyntaxInput _ = SD.recordInferRules
  inferOfflineInput _ = UO.recordInferRules
  termInput _ = recordTermRules
