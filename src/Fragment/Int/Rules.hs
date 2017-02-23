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
import Fragment.Int.Rules.Infer
import Fragment.Int.Rules.Eval

data RInt

instance RulesIn RInt where
  type RuleInferContext e s r m ty pt tm a RInt = IntInferContext e s r m ty pt tm a
  type RuleEvalContext ty pt tm a RInt = IntEvalContext ty pt tm a
  type TypeList RInt = '[TyFInt]
  type ErrorList ty pt tm a RInt = '[ErrUnexpected ty a]
  type PatternList RInt = '[PtFInt]
  type TermList RInt = '[TmFInt]

  inferInput _ = intInferRules
  evalLazyInput _ = intEvalRules
  evalStrictInput _ = intEvalRules
