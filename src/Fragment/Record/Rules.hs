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
  type InferKindContextSyntax e w s r m ki ty a RRecord = KSD.RecordInferKindContext e w s r m ki ty a
  type InferTypeContextSyntax e w s r m ki ty pt tm a RRecord = TSD.RecordInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RRecord = TUO.RecordInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RRecord = RecordTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RRecord = RecordTermContext ki ty pt tm a
  type KindList RRecord = '[KiFBase]
  type TypeList RRecord = '[TyFRecord]
  type ErrorList ki ty pt tm a RRecord = '[ErrExpectedTyRecord ki ty a, ErrRecordNotFound]
  type WarningList ki ty pt tm a RRecord = '[]
  type PatternList RRecord = '[PtFRecord]
  type TermList RRecord = '[TmFRecord]

  inferKindInputSyntax _ = KSD.recordInferKindRules
  inferTypeInputSyntax _ = TSD.recordInferTypeRules
  inferTypeInputOffline _ = TUO.recordInferTypeRules
  typeInput _ = recordTypeRules
  termInput _ = recordTermRules
