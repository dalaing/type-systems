{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Pair.Rules (
    PairContext
  , pairRules
  ) where

import Rules

import Fragment.Pair.Rules.Infer
import Fragment.Pair.Rules.Eval

type PairContext e s r m ty pt tm a = (PairInferContext e s r m ty pt tm a, PairEvalContext ty pt tm a)

pairRules :: PairContext e s r m ty pt tm a
          => RulesInput e s r m ty pt tm a
pairRules =
  RulesInput pairInferRules pairEvalRulesLazy pairEvalRulesStrict
