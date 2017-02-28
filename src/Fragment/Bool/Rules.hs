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
module Fragment.Bool.Rules (
    RBool
  ) where

import Rules
import Ast.Error.Common

import Fragment.Bool.Ast
import qualified Fragment.Bool.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.Bool.Rules.Infer.Unification.Offline as UO
import Fragment.Bool.Rules.Eval

data RBool

instance RulesIn RBool where
  type RuleInferSyntaxContext e w s r m ty pt tm a RBool = SD.BoolInferContext e w s r m ty pt tm a
  type RuleInferOfflineContext e w s r m ty pt tm a RBool = UO.BoolInferContext e w s r m ty pt tm a
  type RuleEvalContext ty pt tm a RBool = BoolEvalContext ty pt tm a
  type TypeList RBool = '[TyFBool]
  type ErrorList ty pt tm a RBool = '[ErrUnexpected ty a]
  type WarningList ty pt tm a RBool = '[]
  type PatternList RBool = '[PtFBool]
  type TermList RBool = '[TmFBool]

  inferSyntaxInput _ = SD.boolInferRules
  inferOfflineInput _ = UO.boolInferRules
  evalLazyInput _ = boolEvalRules
  evalStrictInput _ = boolEvalRules

