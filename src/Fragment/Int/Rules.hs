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
module Fragment.Int.Rules (
    RInt
  ) where

import Rules
import Ast.Error.Common

import Fragment.Int.Ast
import qualified Fragment.Int.Rules.Type.Infer.SyntaxDirected as SD
import qualified Fragment.Int.Rules.Type.Infer.Offline as UO
import Fragment.Int.Rules.Type
import Fragment.Int.Rules.Term

data RInt

instance RulesIn RInt where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RInt = SD.IntInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RInt = UO.IntInferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RInt = IntTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RInt = IntTermContext ki ty pt tm a
  type KindList RInt = '[]
  type TypeList RInt = '[TyFInt]
  type ErrorList ki ty pt tm a RInt = '[ErrUnexpectedType ki ty a]
  type WarningList ki ty pt tm a RInt = '[]
  type PatternList RInt = '[PtFInt]
  type TermList RInt = '[TmFInt]

  inferSyntaxInput _ = SD.intInferRules
  inferOfflineInput _ = UO.intInferRules
  typeInput _ = intTypeRules
  termInput _ = intTermRules
