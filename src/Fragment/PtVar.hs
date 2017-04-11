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
module Fragment.PtVar (
    module X
  , PtVarTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term

import Fragment.PtVar.Helpers as X

import Fragment.PtVar.Rules.Type.Infer.Common
import Fragment.PtVar.Rules.Term

data PtVarTag

instance AstIn PtVarTag where
  type KindList PtVarTag = '[]
  type TypeList PtVarTag = '[]
  type TypeSchemeList PtVarTag = '[]
  type PatternList PtVarTag = '[]
  type TermList PtVarTag = '[]

instance EvalRules e PtVarTag where
  type EvalConstraint ki ty pt tm a e PtVarTag =
    PtVarEvalConstraint ki ty pt tm a

  evalInput _ _ =
    ptVarEvalRules

instance NormalizeRules PtVarTag where
  type NormalizeConstraint ki ty a PtVarTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance MkInferType i => InferTypeRules i PtVarTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i PtVarTag =
    PtVarInferTypeConstraint e w s r m ki ty pt tm a i
  type InferTypeErrorList ki ty pt tm a i PtVarTag =
    '[]
  type InferTypeWarningList ki ty pt tm a i PtVarTag =
    '[]

  inferTypeInput m i _ =
    ptVarInferTypeInput m i
