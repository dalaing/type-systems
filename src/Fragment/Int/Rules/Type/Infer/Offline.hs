{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Int.Rules.Type.Infer.Offline (
    IntInferTypeContext
  , intInferTypeRules
  ) where

import Control.Monad.State (MonadState)
import Control.Lens (review)

import Rules.Type.Infer.Offline
import Ast.Type
import Ast.Type.Var

import Fragment.Int.Ast.Type
import Fragment.Int.Ast.Pattern
import Fragment.Int.Ast.Term

import Fragment.Int.Rules.Type.Infer.Common

createInt :: (MonadState s m, HasTyVarSupply s, ToTyVar a)
          => m (Type ki ty a)
createInt =
  fmap (review _TyVar) freshTyVar

type IntInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyInt ki ty, AsPtInt pt, AsTmInt ki ty pt tm)

intInferTypeRules :: IntInferTypeContext e w s r m ki ty pt tm a
              => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
intInferTypeRules =
  let
    ih = IntHelper createInt expectType
  in
    inferTypeInput ih
