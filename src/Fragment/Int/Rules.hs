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
module Fragment.Int.Rules (
    RInt
  ) where

import Rules
import Ast.Error.Common

import Fragment.Int.Ast
import qualified Fragment.Int.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.Int.Rules.Infer.Unification.Offline as UO
import Fragment.Int.Rules.Eval

data RInt

instance RulesIn RInt where
  type RuleInferSyntaxContext e w s r m ty pt tm a RInt = SD.IntInferContext e w s r m ty pt tm a
  type RuleInferOfflineContext e w s r m ty pt tm a RInt = UO.IntInferContext e w s r m ty pt tm a
  type RuleEvalContext ty pt tm a RInt = IntEvalContext ty pt tm a
  type TypeList RInt = '[TyFInt]
  type ErrorList ty pt tm a RInt = '[ErrUnexpected ty a]
  type WarningList ty pt tm a RInt = '[]
  type PatternList RInt = '[PtFInt]
  type TermList RInt = '[TmFInt]

  inferSyntaxInput _ = SD.intInferRules
  inferOfflineInput _ = UO.intInferRules
  evalLazyInput _ = intEvalRules
  evalStrictInput _ = intEvalRules
