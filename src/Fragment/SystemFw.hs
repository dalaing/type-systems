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
module Fragment.SystemFw (
    module X
  , SystemFwTag
  ) where

import Ast
import Rules.Type
import Rules.Kind.Infer.Common
import Rules.Term

import Fragment.SystemFw.Ast as X
import Fragment.SystemFw.Helpers as X

import Fragment.KiBase.Ast.Kind
import Fragment.KiArr.Ast.Kind
import Fragment.TyArr.Ast.Type
import Fragment.TyAll.Ast.Type
import Fragment.TyAll.Ast.Error
import Fragment.TmLam.Ast.Term
import Fragment.TmApp.Ast.Term
import Fragment.SystemF.Ast.Term

import Fragment.SystemFw.Rules.Type
import Fragment.SystemFw.Rules.Kind.Infer.Common

data SystemFwTag

instance AstIn SystemFwTag where
  type KindList SystemFwTag = '[KiFBase, KiFArr]
  type TypeList SystemFwTag = '[TyFArr, TyFAll, TyFSystemFw]
  type PatternList SystemFwTag = '[]
  type TermList SystemFwTag = '[TmFLam, TmFApp, TmFSystemF]

instance NormalizeRules SystemFwTag where
  type NormalizeConstraint ki ty a SystemFwTag =
    SystemFwNormalizeConstraint ki ty a

  normalizeInput _ =
    systemFwNormalizeRules

instance MkInferKind i => InferKindRules i SystemFwTag where
  type InferKindConstraint e w s r m ki ty a i SystemFwTag =
    SystemFwInferKindConstraint e w s r m ki ty a i
  type InferKindErrorList ki ty a i SystemFwTag =
    '[]
  type InferKindWarningList ki ty a i SystemFwTag =
    '[]

  inferKindInput m i _ =
    systemFwInferKindInput m i
