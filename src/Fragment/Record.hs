{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Fragment.Record (
    module X
  , RecordTag
  ) where

import Ast
import Rules.Kind.Infer.Common
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term
import Fragment.KiBase.Ast.Kind

import Fragment.Record.Ast as X
import Fragment.Record.Helpers as X

import Fragment.Record.Rules.Kind.Infer.Common
import Fragment.Record.Rules.Type
import Fragment.Record.Rules.Type.Infer.Common
import Fragment.Record.Rules.Term

data RecordTag

instance AstIn RecordTag where
  type KindList RecordTag = '[KiFBase]
  type TypeList RecordTag = '[TyFRecord]
  type PatternList RecordTag = '[PtFRecord]
  type TermList RecordTag = '[TmFRecord]

instance EvalRules EStrict RecordTag where
  type EvalConstraint ki ty pt tm a EStrict RecordTag =
    RecordEvalConstraint ki ty pt tm a

  evalInput _ _ =
    recordEvalRulesStrict

instance EvalRules ELazy RecordTag where
  type EvalConstraint ki ty pt tm a ELazy RecordTag =
    RecordEvalConstraint ki ty pt tm a

  evalInput _ _ =
    recordEvalRulesLazy

instance NormalizeRules RecordTag where
  type NormalizeConstraint ki ty a RecordTag =
    RecordNormalizeConstraint ki ty a

  normalizeInput _ =
    recordNormalizeRules

instance MkInferType i => InferTypeRules i RecordTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i RecordTag =
    RecordInferTypeConstraint e w s r m ki ty pt tm a i
  type InferTypeErrorList ki ty pt tm a i RecordTag =
    '[ ErrExpectedTyRecord ki ty a
     , ErrRecordNotFound
     ]
  type InferTypeWarningList ki ty pt tm a i RecordTag =
    '[]

  inferTypeInput m i _ =
    recordInferTypeInput m i

instance MkInferKind i => InferKindRules i RecordTag where
  type InferKindConstraint e w s r m ki ty a i RecordTag =
    RecordInferKindConstraint e w s r m ki ty a i
  type InferKindErrorList ki ty a i RecordTag =
    '[]
  type InferKindWarningList ki ty a i RecordTag =
    '[]

  inferKindInput m i _ =
    recordInferKindInput m i
