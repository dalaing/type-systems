{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.STLC.Rules (
    STLCContext
  , stlcRules
  ) where

import Rules

import Fragment.STLC.Rules.Infer
import Fragment.STLC.Rules.Eval

type STLCContext e s r m ty pt tm a = (STLCInferContext e s r m ty pt tm a, STLCEvalContext ty pt tm a)

stlcRules :: STLCContext e s r m ty pt tm a
          => RulesInput e s r m ty pt tm a
stlcRules =
  RulesInput stlcInferRules stlcEvalRulesLazy stlcEvalRulesStrict
