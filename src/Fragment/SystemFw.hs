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
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Type.Infer.SyntaxDirected
import Rules.Term

import Fragment.SystemFw.Ast as X
import Fragment.SystemFw.Helpers as X

import Fragment.SystemFw.Rules.Type
import Fragment.SystemFw.Rules.Type.Infer.SyntaxDirected
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

instance NormalizeRules SystemFwTag where
  type NormalizeConstraint ki ty a SystemFwTag =
    SystemFwNormalizeConstraint ki ty a

  normalizeInput _ =
    systemFwNormalizeRules

instance InferTypeRules ISyntax SystemFwTag where
  type InferTypeConstraint e w s r m ki ty pt tm a ISyntax SystemFwTag =
    SystemFwInferTypeConstraint e w s r m ki ty pt tm a
  type ErrorList ki ty pt tm a ISyntax SystemFwTag =
    '[]
  type WarningList ki ty pt tm a ISyntax SystemFwTag =
    '[]

  inferTypeInput' m i _ =
    systemFwInferTypeInput m i
