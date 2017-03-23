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

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term
import Fragment.TmLam.Ast.Term
import Fragment.TmApp.Ast.Term

import Fragment.LC.Ast as X
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

instance NormalizeRules LCTag where
  type NormalizeConstraint ki ty a LCTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance MkInferType i => InferTypeRules i LCTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i LCTag =
    (() :: Constraint)
  type ErrorList ki ty pt tm a i LCTag =
    '[]
  type WarningList ki ty pt tm a i LCTag =
    '[]

  inferTypeInput' m i _ =
    mempty
