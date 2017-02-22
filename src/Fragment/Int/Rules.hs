{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Int.Rules (
    IntContext
  , intRules
  ) where

import Rules

import Fragment.Int.Rules.Infer
import Fragment.Int.Rules.Eval

type IntContext e s r m ty pt tm a = (IntInferContext e s r m ty pt tm a, IntEvalContext ty pt tm a)

intRules :: IntContext e s r m ty pt tm a
         => RulesInput e s r m ty pt tm a
intRules =
  RulesInput intInferRules intEvalRules intEvalRules
