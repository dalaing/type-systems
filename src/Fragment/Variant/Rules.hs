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

import Fragment.KiBase.Ast.Kind

import Fragment.Variant.Ast
import qualified Fragment.Variant.Rules.Kind.Infer.SyntaxDirected as KSD
import qualified Fragment.Variant.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.Variant.Rules.Type.Infer.Offline as TUO
import Fragment.Variant.Rules.Type
import Fragment.Variant.Rules.Term

data RVariant

instance RulesIn RVariant where
  type RuleKindInferSyntaxContext e w s r m ki ty a RVariant = KSD.VariantKindRulesContext e w s r m ki ty a
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RVariant = TSD.VariantInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RVariant = TUO.VariantInferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RVariant = VariantTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RVariant = VariantTermContext ki ty pt tm a
  type KindList RVariant = '[KiFBase]
  type TypeList RVariant = '[TyFVariant]
  type ErrorList ki ty pt tm a RVariant = '[ErrExpectedTyVariant ki ty a, ErrVariantNotFound]
  type WarningList ki ty pt tm a RVariant = '[]
  type PatternList RVariant = '[PtFVariant]
  type TermList RVariant = '[TmFVariant]

  inferKindInputSyntax _ = KSD.variantKindRules
  inferSyntaxInput _ = TSD.variantInferRules
  inferOfflineInput _ = TUO.variantInferRules
  typeInput _ = variantTypeRules
  termInput _ = variantTermRules
