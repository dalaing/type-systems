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

import Rules
import Ast.Error.Common

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Term

import Fragment.If.Ast
import qualified Fragment.If.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.If.Rules.Infer.Unification.Offline as UO
import Fragment.If.Rules.Eval

data RIf

instance RulesIn RIf where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RIf = SD.IfInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RIf = UO.IfInferContext e w s r m ki ty pt tm a
  type RuleEvalContext ki ty pt tm a RIf = IfEvalContext ki ty pt tm a
  type KindList RIf = '[]
  type TypeList RIf = '[TyFBool]
  type ErrorList ki ty pt tm a RIf = '[ErrUnexpected ki ty a, ErrExpectedEq ki ty a]
  type WarningList ki ty pt tm a RIf = '[]
  type PatternList RIf = '[]
  type TermList RIf = '[TmFBool, TmFIf]

  inferSyntaxInput _ = SD.ifInferRules
  inferOfflineInput _ = UO.ifInferRules
  evalLazyInput _ = ifEvalRules
  evalStrictInput _ = ifEvalRules
