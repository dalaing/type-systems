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
module Fragment.TmLam (
    module X
  , TmLamTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term
import Fragment.TyArr.Ast.Type

import Fragment.TmLam.Ast as X
import Fragment.TmLam.Helpers as X

import Fragment.TmLam.Rules.Type.Infer.Common
import Fragment.TmLam.Rules.Term

data TmLamTag

instance AstIn TmLamTag where
  type KindList TmLamTag = '[]
  type TypeList TmLamTag = '[TyFArr]
  type PatternList TmLamTag = '[]
  type TermList TmLamTag = '[TmFLam]

instance EvalRules e TmLamTag where
  type EvalConstraint ki ty pt tm a e TmLamTag =
    TmLamEvalConstraint ki ty pt tm a

  evalInput _ _ =
    tmLamEvalRules

instance NormalizeRules TmLamTag where
  type NormalizeConstraint ki ty a TmLamTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance MkInferType i => InferTypeRules i TmLamTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i TmLamTag =
    TmLamInferTypeConstraint e w s r m ki ty pt tm a i
  type ErrorList ki ty pt tm a i TmLamTag =
    '[ ErrExpectedTmLamAnnotation ]
  type WarningList ki ty pt tm a i TmLamTag =
    '[]

  inferTypeInput m i _ =
    tmLamInferTypeInput m i
