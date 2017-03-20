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
module Fragment.TmApp (
    module X
  , TmAppTag
  ) where

import Ast
import Rules.Term

import Fragment.TmApp.Ast as X
import Fragment.TmApp.Rules as X
import Fragment.TmApp.Helpers as X

import Fragment.TmApp.Rules.Term

data TmAppTag

instance AstIn TmAppTag where
  type KindList TmAppTag = '[]
  type TypeList TmAppTag = '[]
  type PatternList TmAppTag = '[]
  type TermList TmAppTag = '[TmFApp]

instance EvalRules EStrict TmAppTag where
  type EvalConstraint ki ty pt tm a EStrict TmAppTag =
    TmAppEvalConstraint ki ty pt tm a

  evalInput _ _ =
    tmAppEvalRulesStrict

instance EvalRules ELazy TmAppTag where
  type EvalConstraint ki ty pt tm a ELazy TmAppTag =
    TmAppEvalConstraint ki ty pt tm a

  evalInput _ _ =
    tmAppEvalRulesLazy
