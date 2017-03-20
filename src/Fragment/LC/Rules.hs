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
module Fragment.LC.Rules (
    RLC
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules

import Fragment.TmLam.Ast.Term
import Fragment.TmApp.Ast.Term

data RLC

instance AstIn RLC where
  type KindList RLC = '[]
  type TypeList RLC = '[]
  type PatternList RLC = '[]
  type TermList RLC = '[TmFLam, TmFApp]

instance RulesIn RLC where
  type InferKindContextSyntax e w s r m ki ty a RLC =
    (() :: Constraint)
  type InferTypeContextSyntax e w s r m ki ty pt tm a RLC =
    (() :: Constraint)
  type InferTypeContextOffline e w s r m ki ty pt tm a RLC =
    (() :: Constraint)
  type RuleTypeContext ki ty a RLC =
    (() :: Constraint)
  type ErrorList ki ty pt tm a RLC = '[]
  type WarningList ki ty pt tm a RLC = '[]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = mempty
  inferTypeInputOffline _ = mempty
  typeInput _ = mempty
