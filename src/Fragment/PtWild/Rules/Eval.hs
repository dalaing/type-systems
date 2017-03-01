{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtWild.Rules.Eval (
    PtWildEvalContext
  , ptWildEvalRules
  ) where

import Control.Lens (preview)

import Rules.Eval

import Ast.Pattern
import Ast.Term

matchWild :: AsPtWild pt => Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
matchWild p _ = do
  _ <- preview _PtWild p
  return []

type PtWildEvalContext ki ty pt tm a = (EvalContext ki ty pt tm a, AsPtWild pt)

ptWildEvalRules :: PtWildEvalContext ki ty pt tm a
               => EvalInput ki ty pt tm a
ptWildEvalRules =
  EvalInput
    []
    []
    [ MatchBase matchWild ]
