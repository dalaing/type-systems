{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Variant.Rules (
    VariantContext
  , variantRules
  ) where

import Rules

import Fragment.Variant.Rules.Infer
import Fragment.Variant.Rules.Eval

type VariantContext e s r m ty pt tm a = (VariantInferContext e s r m ty pt tm a, VariantEvalContext ty pt tm a)

variantRules :: VariantContext e s r m ty pt tm a
            => RulesInput e s r m ty pt tm a
variantRules =
  RulesInput variantInferRules variantEvalRules variantEvalRules
