{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtVar.Rules.Term (
    PtVarEvalConstraint
  , ptVarEvalRules
  ) where

import Control.Lens (preview)

import Rules.Term

import Ast.Pattern
import Ast.Term

matchVar :: Pattern pt a -> Term ki ty pt tm a -> Maybe [Term ki ty pt tm a]
matchVar p tm = do
  _ <- preview _PtVar p
  return [tm]

type PtVarEvalConstraint ki ty pt tm a = BasicEvalConstraint ki ty pt tm a

ptVarEvalRules :: PtVarEvalConstraint ki ty pt tm a
               => EvalInput ki ty pt tm a
ptVarEvalRules =
  EvalInput
    []
    []
    [ MatchBase matchVar ]
