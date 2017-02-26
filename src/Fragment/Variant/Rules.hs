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
import Fragment.Variant.Rules.Infer
import Fragment.Variant.Rules.Eval

data RVariant

instance RulesIn RVariant where
  type RuleInferContext e w s r m ty pt tm a RVariant = VariantInferContext e w s r m ty pt tm a
  type RuleEvalContext ty pt tm a RVariant = VariantEvalContext ty pt tm a
  type TypeList RVariant = '[TyFVariant]
  type ErrorList ty pt tm a RVariant = '[ErrExpectedTyVariant ty a, ErrVariantNotFound]
  type WarningList ty pt tm a RVariant = '[]
  type PatternList RVariant = '[PtFVariant]
  type TermList RVariant = '[TmFVariant]

  inferInput _ = variantInferRules
  evalLazyInput _ = variantEvalRules
  evalStrictInput _ = variantEvalRules
