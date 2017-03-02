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
module Fragment.PtWild.Rules (
    RPtWild
  ) where

import Rules
import Ast.Pattern

import qualified Fragment.PtWild.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.PtWild.Rules.Infer.Unification.Offline as UO
import Fragment.PtWild.Rules.Term

data RPtWild

instance RulesIn RPtWild where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RPtWild = SD.PtWildInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RPtWild = UO.PtWildInferContext e w s r m ki ty pt tm a
  type RuleTermContext ki ty pt tm a RPtWild = PtWildTermContext ki ty pt tm a
  type KindList RPtWild = '[]
  type TypeList RPtWild = '[]
  type ErrorList ki ty pt tm a RPtWild = '[]
  type WarningList ki ty pt tm a RPtWild = '[]
  type PatternList RPtWild = '[PtFWild]
  type TermList RPtWild = '[]

  inferSyntaxInput _ = SD.ptWildInferRules
  inferOfflineInput _ = UO.ptWildInferRules
  termInput _ = ptWildTermRules
