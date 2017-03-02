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
module Fragment.STLC.Rules (
    RSTLC
  ) where

import GHC.Exts (Constraint)

import Rules

import Fragment.STLC.Ast
import Fragment.STLC.Rules.Infer.SyntaxDirected
import Fragment.STLC.Rules.Type
import Fragment.STLC.Rules.Term

data RSTLC

instance RulesIn RSTLC where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RSTLC = STLCInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RSTLC = (() :: Constraint)
  type RuleTypeContext ki ty a RSTLC = STLCTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RSTLC = STLCTermContext ki ty pt tm a
  type KindList RSTLC = '[]
  type TypeList RSTLC = '[TyFSTLC]
  type ErrorList ki ty pt tm a RSTLC = '[ErrExpectedTyArr ki ty a]
  type WarningList ki ty pt tm a RSTLC = '[]
  type PatternList RSTLC = '[]
  type TermList RSTLC = '[TmFSTLC]

  inferSyntaxInput _ = stlcInferRules
  inferOfflineInput _ = mempty
  typeInput _ = stlcTypeRules
  termInput _ = stlcTermRules
