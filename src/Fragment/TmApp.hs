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
module Fragment.TmApp (
    module X
  , TmAppTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term

import Fragment.TmApp.Ast as X
import Fragment.TmApp.Helpers as X

import Fragment.TyArr.Ast.Type
import Fragment.TyArr.Ast.Error

import Fragment.TmApp.Rules.Type.Infer.Common
import Fragment.TmApp.Rules.Term

data TmAppTag

instance AstIn TmAppTag where
  type KindList TmAppTag = '[]
  type TypeList TmAppTag = '[TyFArr]
  type TypeSchemeList TmAppTag = '[]
  type PatternList TmAppTag = '[]
  type TermList TmAppTag = '[TmFApp]

instance EvalRules EStrict TmAppTag where
  type EvalConstraint ki ty pt tm a EStrict TmAppTag =
    TmAppEvalConstraint ki ty pt tm a

  evalInput _ _ =
    tmAppEvalRulesStrict

instance EvalRules ELazy TmAppTag where
  type EvalConstraint ki ty pt tm a ELazy TmAppTag =
    TmAppEvalConstraint ki ty pt tm a

  evalInput _ _ =
    tmAppEvalRulesLazy

instance NormalizeRules TmAppTag where
  type NormalizeConstraint ki ty a TmAppTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance MkInferType i => InferTypeRules i TmAppTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i TmAppTag =
    TmAppInferTypeConstraint e w s r m ki ty pt tm a i
  type InferTypeErrorList ki ty pt tm a i TmAppTag =
    '[ ErrExpectedTyArr ki ty a ]
  type InferTypeWarningList ki ty pt tm a i TmAppTag =
    '[]

  inferTypeInput m i _ =
    tmAppInferTypeInput m i
