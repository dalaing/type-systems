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
module Fragment.PtVar.Rules (
    RPtVar
  ) where

import Rules

import qualified Fragment.PtVar.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.PtVar.Rules.Infer.Unification.Offline as UO
import Fragment.PtVar.Rules.Eval

data RPtVar

instance RulesIn RPtVar where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RPtVar = SD.PtVarInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RPtVar = UO.PtVarInferContext e w s r m ki ty pt tm a
  type RuleEvalContext ki ty pt tm a RPtVar = PtVarEvalContext ki ty pt tm a
  type TypeList RPtVar = '[]
  type ErrorList ki ty pt tm a RPtVar = '[]
  type WarningList ki ty pt tm a RPtVar = '[]
  type PatternList RPtVar = '[]
  type TermList RPtVar = '[]

  inferSyntaxInput _ = SD.ptVarInferRules
  inferOfflineInput _ = UO.ptVarInferRules
  evalLazyInput _ = ptVarEvalRules
  evalStrictInput _ = ptVarEvalRules
