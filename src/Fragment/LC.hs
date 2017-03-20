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
module Fragment.LC (
    module X
  , LCTag
  ) where

import Ast
import Rules.Term
import Fragment.TmLam.Ast.Term
import Fragment.TmApp.Ast.Term

import Fragment.LC.Ast as X
import Fragment.LC.Rules as X
import Fragment.LC.Helpers as X

import Fragment.LC.Rules.Term

data LCTag

instance AstIn LCTag where
  type KindList LCTag = '[]
  type TypeList LCTag = '[]
  type PatternList LCTag = '[]
  type TermList LCTag = '[TmFLam, TmFApp]

instance EvalRules EStrict LCTag where
  type EvalConstraint ki ty pt tm a EStrict LCTag =
    LCEvalConstraint ki ty pt tm a

  evalInput _ _ =
    lcEvalRulesStrict

instance EvalRules ELazy LCTag where
  type EvalConstraint ki ty pt tm a ELazy LCTag =
    LCEvalConstraint ki ty pt tm a

  evalInput _ _ =
    lcEvalRulesLazy
