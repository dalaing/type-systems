{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TyArr.Rules (
    RTyArr
  ) where

import GHC.Exts (Constraint)

import Ast
import Ast.Error.Common
import Rules

import Fragment.TyArr.Ast
import Fragment.TyArr.Rules.Kind.Infer.SyntaxDirected
import Fragment.TyArr.Rules.Type.Infer.Offline
import Fragment.TyArr.Rules.Type

data RTyArr

instance AstIn RTyArr where
  type KindList RTyArr = '[]
  type TypeList RTyArr = '[TyFArr]
  type PatternList RTyArr = '[]
  type TermList RTyArr = '[]

instance RulesIn RTyArr where
  type InferKindContextSyntax e w s r m ki ty a RTyArr =
    TyArrInferKindContext e w s r m ki ty a

  type InferTypeContextSyntax e w s r m ki ty pt tm a RTyArr =
    (() :: Constraint)

  type InferTypeContextOffline e w s r m ki ty pt tm a RTyArr =
    TyArrInferTypeContext e w s r m ki ty pt tm a

  type RuleTypeContext ki ty a RTyArr =
    TyArrTypeContext ki ty a

  type RuleTermContext ki ty pt tm a RTyArr =
    (() :: Constraint)

  type ErrorList ki ty pt tm a RTyArr = '[ErrExpectedTyArr ki ty a, ErrUnexpectedKind ki]
  type WarningList ki ty pt tm a RTyArr = '[]

  inferKindInputSyntax _ = tyArrInferKindRules
  inferTypeInputSyntax _ = mempty
  inferTypeInputOffline _ = tyArrInferTypeRules
  typeInput _ = tyArrTypeRules
  termInput _ = mempty
