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
module Fragment.TmVar (
    module X
  , TmVarTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Term

import Fragment.TmVar.Rules as X
import Fragment.TmVar.Helpers as X

data TmVarTag

instance AstIn TmVarTag where
  type KindList TmVarTag = '[]
  type TypeList TmVarTag = '[]
  type PatternList TmVarTag = '[]
  type TermList TmVarTag = '[]

instance EvalRules e TmVarTag where
  type EvalConstraint ki ty pt tm a e TmVarTag =
    (() :: Constraint)

  evalInput _ _ =
    mempty
