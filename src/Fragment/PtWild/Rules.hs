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

import Fragment.PtWild.Rules.Infer
import Fragment.PtWild.Rules.Eval

data RPtWild

instance RulesIn RPtWild where
  type RuleInferContext e s r m ty pt tm a RPtWild = PtWildInferContext e s r m ty pt tm a
  type RuleEvalContext ty pt tm a RPtWild = PtWildEvalContext ty pt tm a
  type TypeList RPtWild = '[]
  type ErrorList ty pt tm a RPtWild = '[]
  type PatternList RPtWild = '[PtFWild]
  type TermList RPtWild = '[]

  inferInput _ = ptWildInferRules
  evalLazyInput _ = ptWildEvalRules
  evalStrictInput _ = ptWildEvalRules
