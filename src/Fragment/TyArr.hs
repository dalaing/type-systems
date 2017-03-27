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
module Fragment.TyArr (
    module X
  , TyArrTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Kind.Infer.Common
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term
import Fragment.KiBase.Ast.Kind

import Fragment.TyArr.Ast as X
import Fragment.TyArr.Helpers as X

import Fragment.TyArr.Rules.Kind.Infer.Common
import Fragment.TyArr.Rules.Type
import Fragment.TyArr.Rules.Type.Infer.Common

data TyArrTag

instance AstIn TyArrTag where
  type KindList TyArrTag = '[KiFBase]
  type TypeList TyArrTag = '[TyFArr]
  type PatternList TyArrTag = '[]
  type TermList TyArrTag = '[]

instance EvalRules e TyArrTag where
  type EvalConstraint ki ty pt tm a e TyArrTag =
    (() :: Constraint)

  evalInput _ _ =
    mempty

instance NormalizeRules TyArrTag where
  type NormalizeConstraint ki ty a TyArrTag =
    TyArrNormalizeConstraint ki ty a

  normalizeInput _ =
    tyArrNormalizeRules

instance MkInferType i => InferTypeRules i TyArrTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i TyArrTag =
    TyArrInferTypeConstraint e w s r m ki ty pt tm a i
  type InferTypeErrorList ki ty pt tm a i TyArrTag =
    '[]
  type InferTypeWarningList ki ty pt tm a i TyArrTag =
    '[]

  inferTypeInput m i _ =
    tyArrInferTypeInput m i

instance MkInferKind i => InferKindRules i TyArrTag where
  type InferKindConstraint e w s r m ki ty a i TyArrTag =
    TyArrInferKindConstraint e w s r m ki ty a i
  type InferKindErrorList ki ty a i TyArrTag =
    '[]
  type InferKindWarningList ki ty a i TyArrTag =
    '[]

  inferKindInput m i _ =
    tyArrInferKindInput m i
