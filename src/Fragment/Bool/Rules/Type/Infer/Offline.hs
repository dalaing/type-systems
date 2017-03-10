{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Bool.Rules.Type.Infer.Offline (
    BoolInferTypeContext
  , boolInferTypeRules
  ) where

import Control.Monad.State (MonadState)
import Control.Lens (review)

import Rules.Type.Infer.Offline
import Ast.Type
import Ast.Type.Var

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

import Fragment.Bool.Rules.Type.Infer.Common

createBool :: (MonadState s m, HasTyVarSupply s, ToTyVar a)
          => m (Type ki ty a)
createBool =
  fmap (review _TyVar) freshTyVar

type BoolInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyBool ki ty, AsPtBool pt, AsTmBool ki ty pt tm)

boolInferTypeRules :: BoolInferTypeContext e w s r m ki ty pt tm a
              => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
boolInferTypeRules =
  let
    bh = BoolHelper createBool expectType
  in
    inferTypeInput bh
