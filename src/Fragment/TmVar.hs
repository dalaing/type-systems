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
module Fragment.TmVar (
    module X
  , TmVarTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Context.Term.Error
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term

import Fragment.TmVar.Helpers as X

import Fragment.TmVar.Rules.Type.Infer.Common

data TmVarTag

instance AstIn TmVarTag where
  type KindList TmVarTag = '[]
  type TypeList TmVarTag = '[]
  type PatternList TmVarTag = '[]
  type TermList TmVarTag = '[]

instance EvalRules e TmVarTag where
  type EvalConstraint ki ty pt tm a e TmVarTag =
    (() :: Constraint)

  evalInput _ _ =
    mempty

instance NormalizeRules TmVarTag where
  type NormalizeConstraint ki ty a TmVarTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance MkInferType i => InferTypeRules i TmVarTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i TmVarTag =
    TmVarInferTypeConstraint e w s r m ki ty pt tm a i
  type InferTypeErrorList ki ty pt tm a i TmVarTag =
    '[ErrUnboundTermVariable a]
  type InferTypeWarningList ki ty pt tm a i TmVarTag =
    '[]

  inferTypeInput m i _ =
    tmVarInferTypeInput m i
