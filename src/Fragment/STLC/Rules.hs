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

import Rules

import Fragment.STLC.Ast
import qualified Fragment.STLC.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.STLC.Rules.Infer.Unification.Offline as UO
import Fragment.STLC.Rules.Term

data RSTLC

instance RulesIn RSTLC where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RSTLC = SD.STLCInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RSTLC = UO.STLCInferContext e w s r m ki ty pt tm a
  type RuleTermContext ki ty pt tm a RSTLC = STLCTermContext ki ty pt tm a
  type KindList RSTLC = '[]
  type TypeList RSTLC = '[TyFSTLC]
  type ErrorList ki ty pt tm a RSTLC = '[ErrExpectedTyArr ki ty a]
  type WarningList ki ty pt tm a RSTLC = '[]
  type PatternList RSTLC = '[]
  type TermList RSTLC = '[TmFSTLC]

  inferSyntaxInput _ = SD.stlcInferRules
  inferOfflineInput _ = UO.stlcInferRules
  termInput _ = stlcTermRules
