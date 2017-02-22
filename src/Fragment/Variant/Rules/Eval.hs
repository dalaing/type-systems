{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Variant.Rules.Eval (
    VariantEvalContext
  , variantEvalRules
  ) where

import Control.Monad (MonadPlus(..))

import Control.Lens (review, preview)

import Rules.Eval
import Ast.Pattern
import Ast.Term

import Fragment.Variant.Ast.Pattern
import Fragment.Variant.Ast.Term

valueVariant :: (AsTmVariant ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
valueVariant valueFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  tm' <- valueFn tmV
  return $ review _TmVariant (l, tm', ty)

stepVariant :: (AsTmVariant ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepVariant stepFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  tm' <- stepFn tmV
  return $ review _TmVariant (l, tm', ty)

matchVariant :: (AsPtVariant pt, AsTmVariant ty pt tm) => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
matchVariant matchFn p tm = do
  (lP, pV) <- preview _PtVariant p
  (lV, tmV, _) <- preview _TmVariant tm
  if lP == lV
  then matchFn pV tmV
  else mzero

type VariantEvalContext ty pt tm a = (EvalContext ty pt tm a, AsPtVariant pt, AsTmVariant ty pt tm)

variantEvalRules :: VariantEvalContext ty pt tm a
                => EvalInput ty pt tm a
variantEvalRules =
  EvalInput
    [ ValueRecurse valueVariant ]
    [ EvalStep stepVariant ]
    [ MatchRecurse matchVariant ]
