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
module Fragment.TmVar.Rules (
    RTmVar
  ) where

import GHC.Exts (Constraint)

import Rules
import Context.Term.Error

import qualified Fragment.TmVar.Rules.Type.Infer.SyntaxDirected as SD
import qualified Fragment.TmVar.Rules.Type.Infer.Offline as UO

data RTmVar

instance RulesIn RTmVar where
  type RuleKindInferSyntaxContext e w s r m ki ty a RTmVar = (() :: Constraint)
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RTmVar = SD.TmVarInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RTmVar = UO.TmVarInferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RTmVar = (() :: Constraint)
  type RuleTermContext ki ty tm pt a RTmVar = (() :: Constraint)
  type KindList RTmVar = '[]
  type TypeList RTmVar = '[]
  type ErrorList ki ty tm pt a RTmVar = '[ErrUnboundTermVariable a]
  type WarningList ki ty tm pt a RTmVar = '[]
  type PatternList RTmVar = '[]
  type TermList RTmVar = '[]

  inferKindInputSyntax _ = mempty
  inferSyntaxInput _ = SD.tmVarInferRules
  inferOfflineInput _ = UO.tmVarInferRules
  typeInput _ = mempty
  termInput _ = mempty
