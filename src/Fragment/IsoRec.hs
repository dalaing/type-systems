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
module Fragment.IsoRec (
    module X
  , IsoRecTag
  ) where

import Ast
import Rules.Term

import Fragment.IsoRec.Ast as X
import Fragment.IsoRec.Rules as X
import Fragment.IsoRec.Helpers as X

import Fragment.IsoRec.Rules.Term

data IsoRecTag

instance AstIn IsoRecTag where
  type KindList IsoRecTag = '[]
  type TypeList IsoRecTag = '[]
  type PatternList IsoRecTag = '[]
  type TermList IsoRecTag = '[TmFIsoRec]

instance EvalRules EStrict IsoRecTag where
  type EvalConstraint ki ty pt tm a EStrict IsoRecTag =
    IsoRecEvalConstraint ki ty pt tm a

  evalInput _ _ =
    isoRecEvalRulesStrict

instance EvalRules ELazy IsoRecTag where
  type EvalConstraint ki ty pt tm a ELazy IsoRecTag =
    IsoRecEvalConstraint ki ty pt tm a

  evalInput _ _ =
    isoRecEvalRulesLazy
