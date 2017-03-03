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
module Fragment.If.Rules (
    RIf
  ) where

import GHC.Exts (Constraint)

import Rules
import Ast.Error.Common

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Term

import Fragment.If.Ast
import qualified Fragment.If.Rules.Type.Infer.SyntaxDirected as SD
import qualified Fragment.If.Rules.Type.Infer.Offline as UO
import Fragment.If.Rules.Term

data RIf

instance RulesIn RIf where
  type RuleKindInferSyntaxContext e w s r m ki ty a RIf = (() :: Constraint)
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RIf = SD.IfInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RIf = UO.IfInferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RIf = (() :: Constraint)
  type RuleTermContext ki ty pt tm a RIf = IfTermContext ki ty pt tm a
  type KindList RIf = '[]
  type TypeList RIf = '[TyFBool]
  type ErrorList ki ty pt tm a RIf = '[ErrUnexpectedType ki ty a, ErrExpectedTypeEq ki ty a]
  type WarningList ki ty pt tm a RIf = '[]
  type PatternList RIf = '[]
  type TermList RIf = '[TmFBool, TmFIf]

  inferKindInputSyntax _ = mempty
  inferSyntaxInput _ = SD.ifInferRules
  inferOfflineInput _ = UO.ifInferRules
  typeInput _ = mempty
  termInput _ = ifTermRules
