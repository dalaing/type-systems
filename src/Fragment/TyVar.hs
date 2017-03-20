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
module Fragment.TyVar (
    module X
  , TyVarTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Term

import Fragment.TyVar.Rules as X
import Fragment.TyVar.Helpers as X

import Fragment.TyVar.Rules.Type

data TyVarTag

instance AstIn TyVarTag where
  type KindList TyVarTag = '[]
  type TypeList TyVarTag = '[]
  type PatternList TyVarTag = '[]
  type TermList TyVarTag = '[]

instance EvalRules e TyVarTag where
  type EvalConstraint ki ty pt tm a e TyVarTag =
    (() :: Constraint)

  evalInput _ _ =
    mempty

instance NormalizeRules TyVarTag where
  type NormalizeConstraint ki ty a TyVarTag =
    BasicNormalizeConstraint ki ty a

  normalizeInput _ =
    tyVarNormalizeRules
