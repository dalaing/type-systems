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

import Ast
import Rules

import Fragment.KiBase.Ast.Kind

import Fragment.Variant.Ast
import qualified Fragment.Variant.Rules.Kind.Infer.SyntaxDirected as KSD
import qualified Fragment.Variant.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.Variant.Rules.Type.Infer.Offline as TUO
import Fragment.Variant.Rules.Type
import Fragment.Variant.Rules.Term

data RVariant

instance AstIn RVariant where
  type KindList RVariant = '[KiFBase]
  type TypeList RVariant = '[TyFVariant]
  type PatternList RVariant = '[PtFVariant]
  type TermList RVariant = '[TmFVariant]

instance RulesIn RVariant where
  type InferKindContextSyntax e w s r m ki ty a RVariant = KSD.VariantInferKindContext e w s r m ki ty a
  type InferTypeContextSyntax e w s r m ki ty pt tm a RVariant = TSD.VariantInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RVariant = TUO.VariantInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RVariant = VariantTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RVariant = VariantTermContext ki ty pt tm a
  type ErrorList ki ty pt tm a RVariant = '[ErrExpectedTyVariant ki ty a, ErrVariantNotFound]
  type WarningList ki ty pt tm a RVariant = '[]

  inferKindInputSyntax _ = KSD.variantInferKindRules
  inferTypeInputSyntax _ = TSD.variantInferTypeRules
  inferTypeInputOffline _ = TUO.variantInferTypeRules
  typeInput _ = variantTypeRules
  termInput _ = variantTermRules
