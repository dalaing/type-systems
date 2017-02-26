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
import Fragment.If.Rules.Infer
import Fragment.If.Rules.Eval

data RIf

instance RulesIn RIf where
  type RuleInferContext e w s r m ty pt tm a RIf = IfInferContext e w s r m ty pt tm a
  type RuleEvalContext ty pt tm a RIf = IfEvalContext ty pt tm a
  type TypeList RIf = '[TyFBool]
  type ErrorList ty pt tm a RIf = '[ErrUnexpected ty a, ErrExpectedEq ty a]
  type WarningList ty pt tm a RIf = '[]
  type PatternList RIf = '[]
  type TermList RIf = '[TmFBool, TmFIf]

  inferInput _ = ifInferRules
  evalLazyInput _ = ifEvalRules
  evalStrictInput _ = ifEvalRules
