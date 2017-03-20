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
module Fragment.PtWild (
    module X
  , PtWildTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Ast.Pattern
import Rules.Type
import Rules.Term

import Fragment.PtWild.Rules as X
import Fragment.PtWild.Helpers as X

import Fragment.PtWild.Rules.Term

data PtWildTag

instance AstIn PtWildTag where
  type KindList PtWildTag = '[]
  type TypeList PtWildTag = '[]
  type PatternList PtWildTag = '[PtFWild]
  type TermList PtWildTag = '[]

instance EvalRules e PtWildTag where
  type EvalConstraint ki ty pt tm a e PtWildTag =
    PtWildEvalConstraint ki ty pt tm a

  evalInput _ _ =
    ptWildEvalRules

instance NormalizeRules PtWildTag where
  type NormalizeConstraint ki ty a PtWildTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty
