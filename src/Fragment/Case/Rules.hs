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
module Fragment.Case.Rules (
    RCase
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules
import Ast.Error.Common
import Context.Term.Error

import Fragment.Case.Ast
import qualified Fragment.Case.Rules.Type.Infer.SyntaxDirected as SD
import qualified Fragment.Case.Rules.Type.Infer.Offline as UO
import Fragment.Case.Rules.Term

data RCase

instance AstIn RCase where
  type KindList RCase = '[]
  type TypeList RCase = '[]
  type PatternList RCase = '[]
  type TermList RCase = '[TmFCase]

instance RulesIn RCase where
  type InferKindContextSyntax e w s r m ki ty a RCase = (() :: Constraint)
  type InferTypeContextSyntax e w s r m ki ty pt tm a RCase = SD.CaseInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RCase = UO.CaseInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RCase = (() :: Constraint)
  type RuleTermContext ki ty pt tm a RCase = CaseTermContext ki ty pt tm a
  type ErrorList ki ty pt tm a RCase = '[ErrExpectedTypeAllEq ki ty a, ErrUnboundTermVariable a, ErrExpectedPattern ki ty pt tm a, ErrDuplicatedPatternVariables a]
  type WarningList ki ty pt tm a RCase = '[WarnUnusedPatternVariables a, WarnShadowingPatternVariables a]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = SD.caseInferTypeRules
  inferTypeInputOffline _ = UO.caseInferTypeRules
  typeInput _ = mempty
  termInput _ = caseTermRules
