{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Bool.Rules (
    BoolContext
  , boolRules
  ) where

import Rules

import Fragment.Bool.Rules.Infer
import Fragment.Bool.Rules.Eval

type BoolContext e s r m ty pt tm a = (BoolInferContext e s r m ty pt tm a, BoolEvalContext ty pt tm a)

boolRules :: BoolContext e s r m ty pt tm a
          => RulesInput e s r m ty pt tm a
boolRules =
  RulesInput boolInferRules boolEvalRules boolEvalRules
