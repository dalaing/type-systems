{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtVar.Rules.Term (
    PtVarTermContext
  , ptVarTermRules
  ) where

import Control.Lens (preview)

import Rules.Term

import Ast.Pattern
import Ast.Term

matchVar :: Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
matchVar p tm = do
  _ <- preview _PtVar p
  return [tm]

type PtVarTermContext ki ty pt tm a = TermContext ki ty pt tm a

ptVarEvalRules :: PtVarTermContext ki ty pt tm a
               => EvalInput ki ty pt tm a
ptVarEvalRules =
  EvalInput
    []
    []
    [ MatchBase matchVar ]

ptVarTermRules :: PtVarTermContext ki ty pt tm a
               => TermInput ki ty pt tm a
ptVarTermRules =
  TermInput ptVarEvalRules ptVarEvalRules
