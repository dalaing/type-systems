{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fragment.TyArr.Rules.Kind.Infer.Common (
    TyArrInferKindConstraint
  , tyArrInferKindInput
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens (review, preview)

import Ast.Kind
import Ast.Type
import Rules.Kind.Infer.Common

import Fragment.KiBase.Ast.Kind
import Fragment.TyArr.Ast.Type

type TyArrInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , AsKiBase ki
  , AsTyArr ki ty
  )

tyArrInferKindInput :: TyArrInferKindConstraint e w s r m ki ty a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferKindInput e w s r m (InferKindMonad ki a m i) ki ty a
tyArrInferKindInput m i =
  InferKindInput
    []
    [ InferKindRecurse $ inferTyArr m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i ]

inferTyArr :: TyArrInferKindConstraint e w s r m ki ty a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy ki
            -> Proxy ty
            -> Proxy a
            -> Proxy i
            -> (Type ki ty a -> InferKindMonad ki a m i (Kind ki a))
            -> Type ki ty a
            -> Maybe (InferKindMonad ki a m i (Kind ki a))
inferTyArr pm pki pty pa pi inferFn ty = do
  (ty1, ty2) <- preview _TyArr ty
  return $ do
    let kib = review _KiBase ()
    mkCheckKind pm pki pty pa pi inferFn ty1 kib
    mkCheckKind pm pki pty pa pi inferFn ty2 kib
    return kib
