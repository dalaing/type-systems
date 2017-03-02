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
module Fragment.SystemF.Rules (
    RSystemF
  ) where

import Rules

import Fragment.SystemF.Ast
import qualified Fragment.SystemF.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.SystemF.Rules.Infer.Unification.Offline as UO
import Fragment.SystemF.Rules.Term

data RSystemF

instance RulesIn RSystemF where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RSystemF = SD.SystemFInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RSystemF = UO.SystemFInferContext e w s r m ki ty pt tm a
  type RuleTermContext ki ty pt tm a RSystemF = SystemFTermContext ki ty pt tm a
  type KindList RSystemF = '[]
  type TypeList RSystemF = '[TyFSystemF]
  type ErrorList ki ty pt tm a RSystemF = '[ErrExpectedTyArr ki ty a, ErrExpectedTyAll ki ty a]
  type WarningList ki ty pt tm a RSystemF = '[]
  type PatternList RSystemF = '[]
  type TermList RSystemF = '[TmFSystemF]

  inferSyntaxInput _ = SD.systemFInferRules
  inferOfflineInput _ = UO.systemFInferRules
  termInput _ = systemFTermRules
