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
import Fragment.Pair.Rules.Infer
import Fragment.Pair.Rules.Eval

data RPair

instance RulesIn RPair where
  type RuleInferContext e w s r m ty pt tm a RPair = PairInferContext e w s r m ty pt tm a
  type RuleEvalContext ty pt tm a RPair = PairEvalContext ty pt tm a
  type TypeList RPair = '[TyFPair]
  type ErrorList ty pt tm a RPair = '[ErrExpectedTyPair ty a]
  type WarningList ty pt tm a RPair = '[]
  type PatternList RPair = '[PtFPair]
  type TermList RPair = '[TmFPair]

  inferInput _ = pairInferRules
  evalLazyInput _ = pairEvalRulesLazy
  evalStrictInput _ = pairEvalRulesStrict
