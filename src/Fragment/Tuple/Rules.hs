{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Tuple.Rules (
    TupleContext
  , tupleRules
  ) where

import Rules

import Fragment.Tuple.Rules.Infer
import Fragment.Tuple.Rules.Eval

type TupleContext e s r m ty pt tm a = (TupleInferContext e s r m ty pt tm a, TupleEvalContext ty pt tm a)

tupleRules :: TupleContext e s r m ty pt tm a
          => RulesInput e s r m ty pt tm a
tupleRules =
  RulesInput tupleInferRules tupleEvalRulesLazy tupleEvalRulesStrict
