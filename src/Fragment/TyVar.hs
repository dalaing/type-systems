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
module Fragment.TyVar (
    module X
  , TyVarTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Kind.Infer.Common
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term

import Fragment.TyVar.Helpers as X

import Fragment.TyVar.Rules.Kind.Infer.Common
import Fragment.TyVar.Rules.Type

data TyVarTag

instance AstIn TyVarTag where
  type KindList TyVarTag = '[]
  type TypeList TyVarTag = '[]
  type TypeSchemeList TyVarTag = '[]
  type PatternList TyVarTag = '[]
  type TermList TyVarTag = '[]

instance EvalRules e TyVarTag where
  type EvalConstraint ki ty pt tm a e TyVarTag =
    (() :: Constraint)

  evalInput _ _ =
    mempty

instance NormalizeRules TyVarTag where
  type NormalizeConstraint ki ty a TyVarTag =
    BasicNormalizeConstraint ki ty a

  normalizeInput _ =
    tyVarNormalizeRules

instance MkInferType i => InferTypeRules i TyVarTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i TyVarTag =
    (() :: Constraint)
  type InferTypeErrorList ki ty pt tm a i TyVarTag =
    '[]
  type InferTypeWarningList ki ty pt tm a i TyVarTag =
    '[]

  inferTypeInput _ _ _ =
    mempty

instance MkInferKind i => InferKindRules i TyVarTag where
  type InferKindConstraint e w s r m ki ty a i TyVarTag =
    TyVarInferKindConstraint e w s r m ki ty a i
  type InferKindErrorList ki ty a i TyVarTag =
    '[]
  type InferKindWarningList ki ty a i TyVarTag =
    '[]

  inferKindInput m i _ =
    tyVarInferKindInput m i
