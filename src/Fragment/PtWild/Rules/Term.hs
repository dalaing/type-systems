{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtWild.Rules.Term (
    PtWildTermContext
  , ptWildTermRules
  ) where

import Control.Lens (preview)

import Rules.Term

import Ast.Pattern
import Ast.Term

matchWild :: AsPtWild pt => Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
matchWild p _ = do
  _ <- preview _PtWild p
  return []

type PtWildTermContext ki ty pt tm a = (TermContext ki ty pt tm a, AsPtWild pt)

ptWildEvalRules :: PtWildTermContext ki ty pt tm a
               => EvalInput ki ty pt tm a
ptWildEvalRules =
  EvalInput
    []
    []
    [ MatchBase matchWild ]

ptWildTermRules :: PtWildTermContext ki ty pt tm a
                => TermInput ki ty pt tm a
ptWildTermRules =
  TermInput ptWildEvalRules ptWildEvalRules
