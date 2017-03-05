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
module Fragment.SystemF.Rules (
    RSystemF
  ) where

import GHC.Exts (Constraint)

import Rules
import Ast.Error.Common

import Fragment.SystemF.Ast
import Fragment.SystemF.Rules.Kind.Infer.SyntaxDirected
import Fragment.SystemF.Rules.Type.Infer.SyntaxDirected
import Fragment.SystemF.Rules.Type
import Fragment.SystemF.Rules.Term

data RSystemF

instance RulesIn RSystemF where
  type RuleKindInferSyntaxContext e w s r m ki ty a RSystemF = SystemFKindRulesContext e w s r m ki ty a
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RSystemF = SystemFInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RSystemF = (() :: Constraint)
  type RuleTypeContext ki ty a RSystemF = SystemFTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RSystemF = SystemFTermContext ki ty pt tm a
  type KindList RSystemF = '[]
  type TypeList RSystemF = '[TyFSystemF]
  type ErrorList ki ty pt tm a RSystemF = '[ErrUnexpectedKind ki, ErrExpectedTyArr ki ty a, ErrExpectedTyAll ki ty a]
  type WarningList ki ty pt tm a RSystemF = '[]
  type PatternList RSystemF = '[]
  type TermList RSystemF = '[TmFSystemF]

  inferKindInputSyntax _ = systemFKindRules
  inferSyntaxInput _ = systemFInferRules
  inferOfflineInput _ = mempty
  typeInput _ = systemFTypeRules
  termInput _ = systemFTermRules
