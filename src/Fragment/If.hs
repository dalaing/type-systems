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
module Fragment.If (
    module X
  , IfTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term
import Fragment.Bool.Ast

import Fragment.If.Ast as X
import Fragment.If.Helpers as X

import Fragment.If.Rules.Type.Infer.Common
import Fragment.If.Rules.Term

data IfTag

instance AstIn IfTag where
  type KindList IfTag = '[]
  type TypeList IfTag = '[TyFBool]
  type PatternList IfTag = '[]
  type TermList IfTag = '[TmFBool, TmFIf]

instance EvalRules e IfTag where
  type EvalConstraint ki ty pt tm a e IfTag =
    IfEvalConstraint ki ty pt tm a

  evalInput _ _ =
    ifEvalRules

instance NormalizeRules IfTag where
  type NormalizeConstraint ki ty a IfTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance MkInferType i => InferTypeRules i IfTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i IfTag =
    IfInferTypeConstraint e w s r m ki ty pt tm a i
  type ErrorList ki ty pt tm a i IfTag =
    '[]
  type WarningList ki ty pt tm a i IfTag =
    '[]

  inferTypeInput' m i _ =
    ifInferTypeInput m i
