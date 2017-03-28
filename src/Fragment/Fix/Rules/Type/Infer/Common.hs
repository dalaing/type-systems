{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.Fix.Rules.Type.Infer.Common (
    FixInferTypeConstraint
  , fixInferTypeInput
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens (preview)
import Control.Monad.Except (MonadError)

import Ast.Type
import Ast.Term

import Fragment.TyArr.Ast.Type
import Fragment.TyArr.Ast.Error
import Fragment.TyArr.Rules.Type.Infer.Common
import Fragment.Fix.Ast.Term

import Rules.Type.Infer.Common

type FixInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , AsTmFix ki ty pt tm
  , AsTyArr ki ty
  , TyArrInferTypeHelper i
  , TyArrInferTypeHelperConstraint e w s r m ki ty a i
  , MonadError e (InferTypeMonad ki ty a m i)
  , AsExpectedTyArr e ki ty a
  )

fixInferTypeInput :: FixInferTypeConstraint e w s r m ki ty pt tm a i
                  => Proxy (MonadProxy e w s r m)
                  -> Proxy i
                  -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
fixInferTypeInput m i =
  InferTypeInput
    [] [ InferTypeRecurse $ inferTmFix m i] []

inferTmFix :: FixInferTypeConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m )
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmFix m i inferFn tm = do
  tmF <- preview _TmFix tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectArr m i tyF
    expectTypeEq m i tyArg tyRet
    return tyRet
