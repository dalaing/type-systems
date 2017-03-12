{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Fix.Rules.Type.Infer.Offline (
    FixInferTypeContext
  , fixInferTypeRules
  ) where

import Control.Lens (review)
import Control.Monad.State (MonadState)

import Ast.Type
import Ast.Type.Var
import Data.Functor.Rec
import Rules.Type.Infer.Offline

import Fragment.TyArr.Ast.Type

import qualified Fragment.Fix.Rules.Type.Infer.Common as F

expectTyArr :: (Eq a, EqRec (ty ki), MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyArr ki ty)
            => Type ki ty a
            -> UnifyT ki ty a m (Type ki ty a, Type ki ty a)
expectTyArr ty = do
  tyArg <- fmap (review _TyVar) freshTyVar
  tyRet <- fmap (review _TyVar) freshTyVar
  let tyArr = review _TyArr (tyArg, tyRet)
  expectTypeEq ty tyArr
  return (tyArg, tyRet)

type FixInferTypeContext e w s r m ki ty pt tm a =
  ( F.FixInferTypeContext e w s r m (UnifyT ki ty a m) ki ty pt tm a
  , Eq a
  , EqRec (ty ki)
  , MonadState s m
  , HasTyVarSupply s
  , ToTyVar a
  )

fixInferTypeRules :: FixInferTypeContext e w s r m ki ty pt tm a
                  => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
fixInferTypeRules =
  let
    fh = F.FixHelper expectTyArr expectTypeEq
  in
    F.inferTypeInput fh
