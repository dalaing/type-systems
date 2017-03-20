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
module Fragment.Bool (
    module X
  , BoolTag
  ) where

import Ast
import Rules.Term
import Fragment.KiBase.Ast.Kind

import Fragment.Bool.Ast as X
import Fragment.Bool.Rules as X
import Fragment.Bool.Helpers as X

import Fragment.Bool.Rules.Term

data BoolTag

instance AstIn BoolTag where
  type KindList BoolTag = '[KiFBase]
  type TypeList BoolTag = '[TyFBool]
  type PatternList BoolTag = '[PtFBool]
  type TermList BoolTag = '[TmFBool]

instance EvalRules e BoolTag where
  type EvalConstraint ki ty pt tm a e BoolTag =
    BoolEvalConstraint ki ty pt tm a

  evalInput _ _ =
    boolEvalRules
