{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module Fragment.Fix.Rules.Type.Infer.Common (
    FixInferTypeContext
  , FixHelper(..)
  , inferTypeInput
  ) where

import Control.Lens (preview)

import Ast.Type
import Ast.Term

import Fragment.TyArr.Ast.Type
import Fragment.Fix.Ast.Term

import Rules.Type.Infer.Common

data FixHelper m ki ty a =
  FixHelper {
    fhExpectTyArr :: Type ki ty a -> m (Type ki ty a, Type ki ty a)
  , fhExpectTypeEq :: Type ki ty a -> Type ki ty a -> m ()
  }

type FixInferTypeContext e w s r (m :: * -> *) mi ki ty pt tm a =
  ( Monad mi
  , AsTyArr ki ty
  , AsTmFix ki ty pt tm
  )

inferTypeInput :: (Monad mi, AsTyArr ki ty, AsTmFix ki ty pt tm)
               => FixHelper mi ki ty a
               -> InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput fh =
  InferTypeInput
    [] [ InferTypeRecurse $ inferTmFix fh] []

inferTmFix :: (Monad m, AsTyArr ki ty, AsTmFix ki ty pt tm)
           => FixHelper m ki ty a
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmFix (FixHelper expectTyArr expectTypeEq) inferFn tm = do
  tmF <- preview _TmFix tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    expectTypeEq tyArg tyRet
    return tyRet
