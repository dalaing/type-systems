{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Rules (
    RulesInput(..)
  , RulesOutput(..)
  , RulesContext
  , prepareRules
  ) where

import Rules.Infer
import Rules.Eval

data RulesInput e s r m ty pt tm a =
  RulesInput {
    riInferInput :: InferInput e s r m ty pt tm a
  , riEvalInputLazy :: EvalInput ty pt tm a
  , riEvalInputStrict :: EvalInput ty pt tm a
  }

instance Monoid (RulesInput e s r m ty pt tm a) where
  mempty =
    RulesInput mempty mempty mempty
  mappend (RulesInput i1 l1 s1) (RulesInput i2 l2 s2) =
    RulesInput (mappend i1 i2) (mappend l1 l2) (mappend s1 s2)

data RulesOutput e s r m ty pt tm a =
  RulesOutput {
    roInferOutput :: InferOutput e s r m ty pt tm a
  , roEvalOutputLazy :: EvalOutput ty pt tm a
  , roEvalOutputStrict :: EvalOutput ty pt tm a
  }

type RulesContext e s r m ty pt tm a = (InferContext e s r m ty pt tm a, EvalContext ty pt tm a)

prepareRules :: RulesContext e s r m ty pt tm a
             => RulesInput e s r m ty pt tm a
             -> RulesOutput e s r m ty pt tm a
prepareRules(RulesInput i l s) =
  RulesOutput (prepareInfer i) (prepareEvalLazy l) (prepareEvalStrict s)
