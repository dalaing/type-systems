{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtVar.Rules.Eval (
    PtVarEvalContext
  , ptVarEvalRules
  ) where

import Control.Lens (preview)

import Rules.Eval

import Ast.Pattern
import Ast.Term

matchVar :: Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
matchVar p tm = do
  _ <- preview _PtVar p
  return [tm]

type PtVarEvalContext ty pt tm a = EvalContext ty pt tm a

ptVarEvalRules :: PtVarEvalContext ty pt tm a
               => EvalInput ty pt tm a
ptVarEvalRules =
  EvalInput
    []
    []
    [ MatchBase matchVar ]
