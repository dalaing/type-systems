{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Fragment.Tuple (
    module X
  , TupleTag
  ) where

import Ast
import Rules.Type
import Rules.Term
import Fragment.KiBase.Ast.Kind

import Fragment.Tuple.Ast as X
import Fragment.Tuple.Rules as X
import Fragment.Tuple.Helpers as X

import Fragment.Tuple.Rules.Type
import Fragment.Tuple.Rules.Term

data TupleTag

instance AstIn TupleTag where
  type KindList TupleTag = '[KiFBase]
  type TypeList TupleTag = '[TyFTuple]
  type PatternList TupleTag = '[PtFTuple]
  type TermList TupleTag = '[TmFTuple]

instance EvalRules EStrict TupleTag where
  type EvalConstraint ki ty pt tm a EStrict TupleTag =
    TupleEvalConstraint ki ty pt tm a

  evalInput _ _ =
    tupleEvalRulesStrict

instance EvalRules ELazy TupleTag where
  type EvalConstraint ki ty pt tm a ELazy TupleTag =
    TupleEvalConstraint ki ty pt tm a

  evalInput _ _ =
    tupleEvalRulesLazy

instance NormalizeRules TupleTag where
  type NormalizeConstraint ki ty a TupleTag =
    TupleNormalizeConstraint ki ty a

  normalizeInput _ =
    tupleNormalizeRules
