{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module Fragment.TmApp.Rules.Type.Infer.Common (
    TmAppInferTypeContext
  , TmAppHelper(..)
  , inferTypeInput
  ) where

import Control.Lens (preview)

import Ast.Type
import Ast.Term

import Fragment.TyArr.Ast.Type
import Fragment.TmApp.Ast.Term

import Rules.Type.Infer.Common

data TmAppHelper m ki ty a =
  TmAppHelper {
    _taExpectTyArr :: Type ki ty a -> m (Type ki ty a, Type ki ty a)
  , _taExpectTypeEq :: Type ki ty a -> Type ki ty a -> m ()
  }

inferTmApp :: (AsTmApp ki ty pt tm, AsTyArr ki ty, Monad m)
           => TmAppHelper m ki ty a
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmApp (TmAppHelper expectTyArr expectTypeEq) inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectTypeEq tyArg tyX
    return tyRet

type TmAppInferTypeContext e w s r (m :: * -> *) mi ki ty pt tm a = (AsTyArr ki ty, AsTmApp ki ty pt tm, Monad mi)

inferTypeInput :: TmAppInferTypeContext e w s r m mi ki ty pt tm a
               => TmAppHelper mi ki ty a
               -> InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput ah =
  InferTypeInput
    []
    [InferTypeRecurse $ inferTmApp ah]
    []

