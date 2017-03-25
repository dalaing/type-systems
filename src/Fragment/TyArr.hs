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
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term
import Fragment.KiBase.Ast.Kind

import Fragment.TyArr.Ast as X
import Fragment.TyArr.Helpers as X

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
  type ErrorList ki ty pt tm a i TyArrTag =
    '[]
  type WarningList ki ty pt tm a i TyArrTag =
    '[]

  inferTypeInput m i _ =
    tyArrInferTypeInput m i
