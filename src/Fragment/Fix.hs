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
module Fragment.Fix (
    module X
  , FixTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Term
import Fragment.TyArr.Ast.Type

import Fragment.Fix.Ast as X
import Fragment.Fix.Rules as X
import Fragment.Fix.Helpers as X

import Fragment.Fix.Rules.Term

data FixTag

instance AstIn FixTag where
  type KindList FixTag = '[]
  type TypeList FixTag = '[TyFArr]
  type PatternList FixTag = '[]
  type TermList FixTag = '[TmFFix]

instance EvalRules e FixTag where
  type EvalConstraint ki ty pt tm a e FixTag =
    FixEvalConstraint ki ty pt tm a

  evalInput _ _ =
    fixEvalRules

instance NormalizeRules FixTag where
  type NormalizeConstraint ki ty a FixTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty
