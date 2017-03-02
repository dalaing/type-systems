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
import Fragment.PtVar.Rules.Term

data RPtVar

instance RulesIn RPtVar where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RPtVar = SD.PtVarInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RPtVar = UO.PtVarInferContext e w s r m ki ty pt tm a
  type RuleTermContext ki ty pt tm a RPtVar = PtVarTermContext ki ty pt tm a
  type KindList RPtVar = '[]
  type TypeList RPtVar = '[]
  type ErrorList ki ty pt tm a RPtVar = '[]
  type WarningList ki ty pt tm a RPtVar = '[]
  type PatternList RPtVar = '[]
  type TermList RPtVar = '[]

  inferSyntaxInput _ = SD.ptVarInferRules
  inferOfflineInput _ = UO.ptVarInferRules
  termInput _ = ptVarTermRules
