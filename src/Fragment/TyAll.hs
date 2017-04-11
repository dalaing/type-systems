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
module Fragment.TyAll (
    module X
  , TyAllTag
  ) where

import Ast
import Rules.Kind.Infer.Common
import Rules.Type

import Fragment.KiArr.Ast.Kind

import Fragment.TyAll.Ast as X
import Fragment.TyAll.Helpers as X

import Fragment.TyAll.Rules.Kind.Infer.Common
import Fragment.TyAll.Rules.Type

data TyAllTag

instance AstIn TyAllTag where
  type KindList TyAllTag = '[KiFArr]
  type TypeList TyAllTag = '[TyFAll]
  type TypeSchemeList TyAllTag = '[]
  type PatternList TyAllTag = '[]
  type TermList TyAllTag = '[]

instance NormalizeRules TyAllTag where
  type NormalizeConstraint ki ty a TyAllTag =
    TyAllNormalizeConstraint ki ty a

  normalizeInput _ =
    tyAllNormalizeRules

instance MkInferKind i => InferKindRules i TyAllTag where
  type InferKindConstraint e w s r m ki ty a i TyAllTag =
    TyAllInferKindConstraint e w s r m ki ty a i
  type InferKindErrorList ki ty a i TyAllTag =
    '[]
  type InferKindWarningList ki ty a i TyAllTag =
    '[]

  inferKindInput m i _ =
    tyAllInferKindInput m i
