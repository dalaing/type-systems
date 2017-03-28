{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TmApp.Rules.Type.Infer.Common (
    TmAppInferTypeConstraint
  , tmAppInferTypeInput
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens (preview)

import Ast.Type
import Ast.Term

import Fragment.TyArr.Ast.Type
import Fragment.TyArr.Rules.Type.Infer.Common
import Fragment.TmApp.Ast.Term

import Rules.Type.Infer.Common

type TmAppInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , TyArrInferTypeHelper i
  , TyArrInferTypeHelperConstraint e w s r m ki ty a i
  , AsTmApp ki ty pt tm
  , AsTyArr ki ty
  )

inferTmApp :: TmAppInferTypeConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmApp m i inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectArr m i tyF
    tyX <- inferFn tmX
    expectTypeEq m i tyArg tyX
    return tyRet

tmAppInferTypeInput :: TmAppInferTypeConstraint e w s r m ki ty pt tm a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
tmAppInferTypeInput m i =
  InferTypeInput
    []
    [InferTypeRecurse $ inferTmApp m i]
    []

