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
module Fragment.Variant.Rules (
    RVariant
  ) where

import Rules

import Fragment.Variant.Ast
import qualified Fragment.Variant.Rules.Type.Infer.SyntaxDirected as SD
import qualified Fragment.Variant.Rules.Type.Infer.Offline as UO
import Fragment.Variant.Rules.Type
import Fragment.Variant.Rules.Term

data RVariant

instance RulesIn RVariant where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RVariant = SD.VariantInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RVariant = UO.VariantInferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RVariant = VariantTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RVariant = VariantTermContext ki ty pt tm a
  type KindList RVariant = '[]
  type TypeList RVariant = '[TyFVariant]
  type ErrorList ki ty pt tm a RVariant = '[ErrExpectedTyVariant ki ty a, ErrVariantNotFound]
  type WarningList ki ty pt tm a RVariant = '[]
  type PatternList RVariant = '[PtFVariant]
  type TermList RVariant = '[TmFVariant]

  inferSyntaxInput _ = SD.variantInferRules
  inferOfflineInput _ = UO.variantInferRules
  typeInput _ = variantTypeRules
  termInput _ = variantTermRules
