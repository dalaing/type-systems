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
module Fragment.Fix (
    module X
  , FixTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term

import Fragment.Fix.Ast as X
import Fragment.Fix.Helpers as X

import Fragment.TyArr.Ast.Type
import Fragment.TyArr.Ast.Error
import Fragment.TmLam.Ast.Term

import Fragment.Fix.Rules.Term
import Fragment.Fix.Rules.Type.Infer.Common

data FixTag

instance AstIn FixTag where
  type KindList FixTag = '[]
  type TypeList FixTag = '[TyFArr]
  type PatternList FixTag = '[]
  type TermList FixTag = '[TmFFix, TmFLam]

instance EvalRules e FixTag where
  type EvalConstraint ki ty pt tm a e FixTag =
    FixEvalConstraint ki ty pt tm a

  evalInput _ _ =
    fixEvalRules

instance NormalizeRules FixTag where
  type NormalizeConstraint ki ty a FixTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance MkInferType i => InferTypeRules i FixTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i FixTag =
    FixInferTypeConstraint e w s r m ki ty pt tm a i
  type ErrorList ki ty pt tm a i FixTag =
    '[ ErrExpectedTyArr ki ty a ]
  type WarningList ki ty pt tm a i FixTag =
    '[]

  inferTypeInput' m i _ =
    fixInferTypeInput m i
