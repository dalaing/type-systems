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
module Fragment.SystemF (
    module X
  , SystemFTag
  ) where

import Ast
import Rules.Term

import Fragment.SystemF.Ast as X
import Fragment.SystemF.Rules as X
import Fragment.SystemF.Helpers as X

import Fragment.SystemF.Rules.Term

data SystemFTag

instance AstIn SystemFTag where
  type KindList SystemFTag = '[]
  type TypeList SystemFTag = '[TyFSystemF]
  type PatternList SystemFTag = '[]
  type TermList SystemFTag = '[TmFSystemF]

instance EvalRules EStrict SystemFTag where
  type EvalConstraint ki ty pt tm a EStrict SystemFTag =
    SystemFEvalConstraint ki ty pt tm a

  evalInput _ _ =
    systemFEvalRulesStrict

instance EvalRules ELazy SystemFTag where
  type EvalConstraint ki ty pt tm a ELazy SystemFTag =
    SystemFEvalConstraint ki ty pt tm a

  evalInput _ _ =
    systemFEvalRulesLazy
