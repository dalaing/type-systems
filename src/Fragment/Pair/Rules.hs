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
module Fragment.Pair.Rules (
    RPair
  ) where

import Rules

import Fragment.Pair.Ast
import qualified Fragment.Pair.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.Pair.Rules.Infer.Unification.Offline as UO
import Fragment.Pair.Rules.Eval

data RPair

instance RulesIn RPair where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RPair = SD.PairInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RPair = UO.PairInferContext e w s r m ki ty pt tm a
  type RuleEvalContext ki ty pt tm a RPair = PairEvalContext ki ty pt tm a
  type KindList RPair = '[]
  type TypeList RPair = '[TyFPair]
  type ErrorList ki ty pt tm a RPair = '[ErrExpectedTyPair ki ty a]
  type WarningList ki ty pt tm a RPair = '[]
  type PatternList RPair = '[PtFPair]
  type TermList RPair = '[TmFPair]

  inferSyntaxInput _ = SD.pairInferRules
  inferOfflineInput _ = UO.pairInferRules
  evalLazyInput _ = pairEvalRulesLazy
  evalStrictInput _ = pairEvalRulesStrict
