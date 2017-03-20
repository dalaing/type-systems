{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtWild.Rules.Term (
    PtWildEvalConstraint
  , ptWildEvalRules
  ) where

import Control.Lens (preview)

import Rules.Term

import Ast.Pattern
import Ast.Term

matchWild :: AsPtWild pt => Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
matchWild p _ = do
  _ <- preview _PtWild p
  return []

type PtWildEvalConstraint ki ty pt tm a =
  ( BasicEvalConstraint ki ty pt tm a
  , AsPtWild pt
  )

ptWildEvalRules :: PtWildEvalConstraint ki ty pt tm a
                => EvalInput ki ty pt tm a
ptWildEvalRules =
  EvalInput
    []
    []
    [ MatchBase matchWild ]
