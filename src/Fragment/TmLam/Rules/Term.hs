{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.TmLam.Rules.Term (
    TmLamTermContext
  , tmLamTermRules
  ) where

import Control.Lens (preview)

import Rules.Term
import Ast.Term

import Fragment.TmLam.Ast.Term

valTmLam :: AsTmLam ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
valTmLam tm = do
  _ <- preview _TmLam tm
  return tm

type TmLamTermContext ki ty pt tm a = (TermContext ki ty pt tm a, AsTmLam ki ty pt tm)

tmLamEvalRules :: TmLamTermContext ki ty pt tm a
               => EvalInput ki ty pt tm a
tmLamEvalRules =
  EvalInput
  [ ValueBase valTmLam ]
  []
  []

tmLamTermRules :: TmLamTermContext ki ty pt tm a
               => TermInput ki ty pt tm a
tmLamTermRules =
  TermInput tmLamEvalRules tmLamEvalRules
