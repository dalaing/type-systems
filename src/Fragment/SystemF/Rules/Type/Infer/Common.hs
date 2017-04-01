{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fragment.SystemF.Rules.Type.Infer.Common (
    SystemFInferTypeConstraint
  , systemFInferTypeInput
  ) where

import Data.Proxy (Proxy)

import Bound (instantiate1)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped)
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState)

import Ast.Term
import Ast.Type
import Ast.Type.Var
import Rules.Type.Infer.Common

import Fragment.TyAll.Ast.Type
import Fragment.TyAll.Ast.Error

import Fragment.SystemF.Ast.Term

type SystemFInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , MkInferType i
  , MkInferTypeConstraint e w s r m ki ty a i
  , AsTmSystemF ki ty pt tm
  , AsTyAll ki ty
  , MonadState s (InferTypeMonad ki ty a m i)
  , HasTyVarSupply s
  , ToTyVar a
  , Eq a
  , MonadError e (InferTypeMonad ki ty a m i)
  , AsExpectedTyAll e ki ty a
  )

inferTmLamTy :: SystemFInferTypeConstraint e w s r m ki ty pt tm a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
             -> Term ki ty pt tm a
             -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmLamTy _ _ inferFn tm = do
  (mki, tmF) <- preview _TmLamTy tm
  return $ do
    v <- freshTyVar
    ty <- inferFn (review _Wrapped . instantiate1 (review (_TmAstVar . _TmAstTyVar) v) $ tmF)
    return . review _TyAll $ (mki, abstractTy v ty)

inferTmAppTy :: SystemFInferTypeConstraint e w s r m ki ty pt tm a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
             -> Term ki ty pt tm a
             -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmAppTy _ _ inferFn tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  return $ do
    tyF <- inferFn tmF
    (_ , s) <- expectTyAll tyF
    return $ instantiateTy tyX s

systemFInferTypeInput :: SystemFInferTypeConstraint e w s r m ki ty pt tm a i
                      => Proxy (MonadProxy e w s r m)
                      -> Proxy i
                      -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
systemFInferTypeInput m i =
  InferTypeInput
    []
    [ InferTypeRecurse $ inferTmLamTy m i
    , InferTypeRecurse $ inferTmAppTy m i
    ]
    []
