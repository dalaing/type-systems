{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Fragment.Variant (
    module X
  , VariantTag
  ) where

import Ast
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term
import Fragment.KiBase.Ast.Kind

import Fragment.Variant.Ast as X
import Fragment.Variant.Helpers as X

import Fragment.Variant.Rules.Type
import Fragment.Variant.Rules.Type.Infer.Common
import Fragment.Variant.Rules.Term

data VariantTag

instance AstIn VariantTag where
  type KindList VariantTag = '[KiFBase]
  type TypeList VariantTag = '[TyFVariant]
  type PatternList VariantTag = '[PtFVariant]
  type TermList VariantTag = '[TmFVariant]

instance EvalRules e VariantTag where
  type EvalConstraint ki ty pt tm a e VariantTag =
    VariantEvalConstraint ki ty pt tm a

  evalInput _ _ =
    variantEvalRules

instance NormalizeRules VariantTag where
  type NormalizeConstraint ki ty a VariantTag =
    VariantNormalizeConstraint ki ty a

  normalizeInput _ =
    variantNormalizeRules

instance MkInferType i => InferTypeRules i VariantTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i VariantTag =
    VariantInferTypeConstraint e w s r m ki ty pt tm a i
  type ErrorList ki ty pt tm a i VariantTag =
    '[ ErrExpectedTyVariant ki ty a
     , ErrVariantNotFound
     ]
  type WarningList ki ty pt tm a i VariantTag =
    '[]

  inferTypeInput' m i _ =
    variantInferTypeInput m i
