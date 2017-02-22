{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.If.Rules (
    IfContext
  , ifRules
  ) where

import Rules

import Fragment.If.Rules.Infer
import Fragment.If.Rules.Eval

type IfContext e s r m ty pt tm a = (IfInferContext e s r m ty pt tm a, IfEvalContext ty pt tm a)

ifRules :: IfContext e s r m ty pt tm a
          => RulesInput e s r m ty pt tm a
ifRules =
  RulesInput ifInferRules ifEvalRules ifEvalRules
