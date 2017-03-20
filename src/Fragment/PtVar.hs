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
module Fragment.PtVar (
    module X
  , PtVarTag
  ) where

import Ast
import Rules.Term

import Fragment.PtVar.Rules as X
import Fragment.PtVar.Helpers as X

import Fragment.PtVar.Rules.Term

data PtVarTag

instance AstIn PtVarTag where
  type KindList PtVarTag = '[]
  type TypeList PtVarTag = '[]
  type PatternList PtVarTag = '[]
  type TermList PtVarTag = '[]

instance EvalRules e PtVarTag where
  type EvalConstraint ki ty pt tm a e PtVarTag =
    PtVarEvalConstraint ki ty pt tm a

  evalInput _ _ =
    ptVarEvalRules
