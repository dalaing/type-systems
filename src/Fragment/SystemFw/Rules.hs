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
module Fragment.SystemFw.Rules (
    RSystemFw
  ) where

import GHC.Exts (Constraint)

import Rules
import Ast.Error.Common
import Context.Type.Error

import Fragment.SystemFw.Ast
import Fragment.SystemFw.Rules.Kind.Infer.SyntaxDirected
import Fragment.SystemFw.Rules.Type.Infer.SyntaxDirected
import Fragment.SystemFw.Rules.Type
import Fragment.SystemFw.Rules.Term

data RSystemFw

instance RulesIn RSystemFw where
  type InferKindContextSyntax e w s r m ki ty a RSystemFw = SystemFwInferKindContext e w s r m ki ty a
  type InferTypeContextSyntax e w s r m ki ty pt tm a RSystemFw = SystemFwInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RSystemFw = (() :: Constraint)
  type RuleTypeContext ki ty a RSystemFw = SystemFwTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RSystemFw = SystemFwTermContext ki ty pt tm a
  type KindList RSystemFw = '[KiFSystemFw]
  type TypeList RSystemFw = '[TyFSystemFw]
  type ErrorList ki ty pt tm a RSystemFw = '[ErrUnexpectedKind ki, ErrExpectedKiArr ki, ErrExpectedKindEq ki, ErrExpectedTyArr ki ty a, ErrExpectedTyAll ki ty a, ErrUnboundTypeVariable a]
  type WarningList ki ty pt tm a RSystemFw = '[]
  type PatternList RSystemFw = '[]
  type TermList RSystemFw = '[TmFSystemFw]

  inferKindInputSyntax _ = systemFwInferKindRules
  inferTypeInputSyntax _ = systemFwInferTypeRules
  inferTypeInputOffline _ = mempty
  typeInput _ = systemFwTypeRules
  termInput _ = systemFwTermRules
