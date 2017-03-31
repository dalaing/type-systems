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
module Fragment.KiArr (
    module X
  , KiArrTag
  ) where

import Ast
import Rules.Kind.Infer.Common

import Fragment.KiArr.Ast as X
import Fragment.KiArr.Helpers as X

import Fragment.KiArr.Rules.Kind.Infer.Common

data KiArrTag

instance AstIn KiArrTag where
  type KindList KiArrTag = '[KiFArr]
  type TypeList KiArrTag = '[]
  type PatternList KiArrTag = '[]
  type TermList KiArrTag = '[]

instance MkInferKind i => InferKindRules i KiArrTag where
  type InferKindConstraint e w s r m ki ty a i KiArrTag =
    KiArrInferKindConstraint e w s r m ki ty a i
  type InferKindErrorList ki ty a i KiArrTag =
    '[]
  type InferKindWarningList ki ty a i KiArrTag =
    '[]

  inferKindInput m i _ =
    kiArrInferKindInput m i
