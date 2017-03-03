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

import Rules

import Fragment.TyVar.Rules.Type

data RTyVar

instance RulesIn RTyVar where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RTyVar = (() :: Constraint)
  type RuleInferOfflineContext e w s r m ki ty pt tm a RTyVar = (() :: Constraint)
  type RuleTypeContext ki ty a RTyVar = (TyVarTypeContext ki ty a)
  type RuleTermContext ki ty tm pt a RTyVar = (() :: Constraint)
  type KindList RTyVar = '[]
  type TypeList RTyVar = '[]
  type ErrorList ki ty tm pt a RTyVar = '[]
  type WarningList ki ty tm pt a RTyVar = '[]
  type PatternList RTyVar = '[]
  type TermList RTyVar = '[]

  inferSyntaxInput _ = mempty
  inferOfflineInput _ = mempty
  typeInput _ = tyVarTypeRules
  termInput _ = mempty
