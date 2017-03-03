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

import Fragment.KiBase.Ast.Kind

import Fragment.Record.Ast
import qualified Fragment.Record.Rules.Kind.Infer.SyntaxDirected as KSD
import qualified Fragment.Record.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.Record.Rules.Type.Infer.Offline as TUO
import Fragment.Record.Rules.Type
import Fragment.Record.Rules.Term

data RRecord

instance RulesIn RRecord where
  type RuleKindInferSyntaxContext e w s r m ki ty a RRecord = KSD.RecordKindRulesContext e w s r m ki ty a
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RRecord = TSD.RecordInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RRecord = TUO.RecordInferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RRecord = RecordTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RRecord = RecordTermContext ki ty pt tm a
  type KindList RRecord = '[KiFBase]
  type TypeList RRecord = '[TyFRecord]
  type ErrorList ki ty pt tm a RRecord = '[ErrExpectedTyRecord ki ty a, ErrRecordNotFound]
  type WarningList ki ty pt tm a RRecord = '[]
  type PatternList RRecord = '[PtFRecord]
  type TermList RRecord = '[TmFRecord]

  inferKindInputSyntax _ = KSD.recordKindRules
  inferSyntaxInput _ = TSD.recordInferRules
  inferOfflineInput _ = TUO.recordInferRules
  typeInput _ = recordTypeRules
  termInput _ = recordTermRules
