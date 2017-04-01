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
module Fragment.SystemF (
    module X
  , SystemFTag
  ) where

import Ast
import Rules.Type.Infer.Common
import Rules.Term

import Fragment.SystemF.Ast as X
import Fragment.SystemF.Helpers as X

import Fragment.KiBase.Ast.Kind
import Fragment.KiArr.Ast.Kind
import Fragment.TyArr.Ast.Type
import Fragment.TyAll.Ast.Type
import Fragment.TyAll.Ast.Error
import Fragment.TmLam.Ast.Term
import Fragment.TmApp.Ast.Term

import Fragment.SystemF.Rules.Type.Infer.Common
import Fragment.SystemF.Rules.Term

data SystemFTag

instance AstIn SystemFTag where
  type KindList SystemFTag = '[KiFBase, KiFArr]
  type TypeList SystemFTag = '[TyFArr, TyFAll]
  type PatternList SystemFTag = '[]
  type TermList SystemFTag = '[TmFLam, TmFApp, TmFSystemF]

instance EvalRules e SystemFTag where
  type EvalConstraint ki ty pt tm a e SystemFTag =
    SystemFEvalConstraint ki ty pt tm a

  evalInput _ _ =
    systemFEvalRules

instance MkInferType i => InferTypeRules i SystemFTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i SystemFTag =
    SystemFInferTypeConstraint e w s r m ki ty pt tm a i
  type InferTypeErrorList ki ty pt tm a i SystemFTag =
    '[ ErrExpectedTyAll ki ty a ]
  type InferTypeWarningList ki ty pt tm a i SystemFTag =
    '[]

  inferTypeInput m i _ =
    systemFInferTypeInput m i
