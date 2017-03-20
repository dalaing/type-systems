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
module Fragment.TyVar.Rules (
    RTyVar
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules

import Fragment.TyVar.Rules.Kind.Infer.SyntaxDirected
import Fragment.TyVar.Rules.Type

data RTyVar

instance AstIn RTyVar where
  type KindList RTyVar = '[]
  type TypeList RTyVar = '[]
  type PatternList RTyVar = '[]
  type TermList RTyVar = '[]

instance RulesIn RTyVar where
  type InferKindContextSyntax e w s r m ki ty a RTyVar = TyVarInferKindContext e w s r m ki ty a
  type InferTypeContextSyntax e w s r m ki ty pt tm a RTyVar = (() :: Constraint)
  type InferTypeContextOffline e w s r m ki ty pt tm a RTyVar = (() :: Constraint)
  type RuleTypeContext ki ty a RTyVar = (TyVarTypeContext ki ty a)
  type ErrorList ki ty tm pt a RTyVar = '[]
  type WarningList ki ty tm pt a RTyVar = '[]

  inferKindInputSyntax _ = tyVarInferKindRules
  inferTypeInputSyntax _ = mempty
  inferTypeInputOffline _ = mempty
  typeInput _ = tyVarTypeRules
