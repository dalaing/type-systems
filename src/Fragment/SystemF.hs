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
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Type.Infer.SyntaxDirected
import Rules.Term

import Fragment.SystemF.Ast as X
import Fragment.SystemF.Helpers as X

import Fragment.SystemF.Rules.Type
import Fragment.SystemF.Rules.Type.Infer.SyntaxDirected
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

instance NormalizeRules SystemFTag where
  type NormalizeConstraint ki ty a SystemFTag =
    SystemFNormalizeConstraint ki ty a

  normalizeInput _ =
    systemFNormalizeRules

instance InferTypeRules ITSyntax SystemFTag where
  type InferTypeConstraint e w s r m ki ty pt tm a ITSyntax SystemFTag =
    SystemFInferTypeConstraint e w s r m ki ty pt tm a
  type ErrorList ki ty pt tm a ITSyntax SystemFTag =
    '[ ErrExpectedTyArr ki ty a
     , ErrExpectedTyAll ki ty a
     ]
  type WarningList ki ty pt tm a ITSyntax SystemFTag =
    '[]


  inferTypeInput m i _ =
    systemFInferTypeInput m i
