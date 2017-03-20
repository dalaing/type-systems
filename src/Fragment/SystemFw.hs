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
module Fragment.SystemFw (
    module X
  , SystemFwTag
  ) where

import Ast
import Rules.Term
import Fragment.KiBase.Ast.Kind

import Fragment.SystemFw.Ast as X
import Fragment.SystemFw.Rules as X
import Fragment.SystemFw.Helpers as X

import Fragment.SystemFw.Rules.Term

data SystemFwTag

instance AstIn SystemFwTag where
  type KindList SystemFwTag = '[KiFSystemFw]
  type TypeList SystemFwTag = '[TyFSystemFw]
  type PatternList SystemFwTag = '[]
  type TermList SystemFwTag = '[TmFSystemFw]

instance EvalRules EStrict SystemFwTag where
  type EvalConstraint ki ty pt tm a EStrict SystemFwTag =
    SystemFwEvalConstraint ki ty pt tm a

  evalInput _ _ =
    systemFwEvalRulesStrict

instance EvalRules ELazy SystemFwTag where
  type EvalConstraint ki ty pt tm a ELazy SystemFwTag =
    SystemFwEvalConstraint ki ty pt tm a

  evalInput _ _ =
    systemFwEvalRulesLazy
