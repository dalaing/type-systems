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
module Fragment.Bool (
    module X
  , BoolTag
  ) where

import Ast
import Rules.Kind.Infer.Common
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term
import Fragment.KiBase.Ast.Kind

import Fragment.Bool.Ast as X
import Fragment.Bool.Helpers as X

import Fragment.Bool.Rules.Kind.Infer.Common
import Fragment.Bool.Rules.Type
import Fragment.Bool.Rules.Type.Infer.Common
import Fragment.Bool.Rules.Term

data BoolTag

instance AstIn BoolTag where
  type KindList BoolTag = '[KiFBase]
  type TypeList BoolTag = '[TyFBool]
  type TypeSchemeList BoolTag = '[]
  type PatternList BoolTag = '[PtFBool]
  type TermList BoolTag = '[TmFBool]

instance EvalRules e BoolTag where
  type EvalConstraint ki ty pt tm a e BoolTag =
    BoolEvalConstraint ki ty pt tm a

  evalInput _ _ =
    boolEvalRules

instance NormalizeRules BoolTag where
  type NormalizeConstraint ki ty a BoolTag =
    BoolNormalizeConstraint ki ty a

  normalizeInput _ =
    boolNormalizeRules

instance MkInferType i => InferTypeRules i BoolTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i BoolTag =
    BoolInferTypeConstraint e w s r m ki ty pt tm a i
  type InferTypeErrorList ki ty pt tm a i BoolTag =
    '[]
  type InferTypeWarningList ki ty pt tm a i BoolTag =
    '[]

  inferTypeInput m i _ =
    boolInferTypeInput m i

instance MkInferKind i => InferKindRules i BoolTag where
  type InferKindConstraint e w s r m ki ty a i BoolTag =
    BoolInferKindConstraint e w s r m ki ty a i
  type InferKindErrorList ki ty a i BoolTag =
    '[]
  type InferKindWarningList ki ty a i BoolTag =
    '[]

  inferKindInput m i _ =
    boolInferKindInput m i
