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
import Rules.Term
import Fragment.KiBase.Ast.Kind

import Fragment.Variant.Ast as X
import Fragment.Variant.Rules as X
import Fragment.Variant.Helpers as X

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
