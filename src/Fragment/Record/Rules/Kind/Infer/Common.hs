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
module Fragment.Record.Rules.Kind.Infer.Common (
    RecordInferKindConstraint
  , recordInferKindInput
  ) where

import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))

import Control.Lens (review, preview)

import Ast.Kind
import Ast.Type
import Rules.Kind.Infer.Common

import Fragment.KiBase.Ast.Kind
import Fragment.Record.Ast.Type

type RecordInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , AsKiBase ki
  , AsTyRecord ki ty
  )

recordInferKindInput :: RecordInferKindConstraint e w s r m ki ty a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferKindInput e w s r m (InferKindMonad m ki a i) ki ty a
recordInferKindInput m i =
  InferKindInput
    []
    [ InferKindRecurse $ inferTyRecord m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i ]

inferTyRecord :: RecordInferKindConstraint e w s r m ki ty a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy ki
            -> Proxy ty
            -> Proxy a
            -> Proxy i
            -> (Type ki ty a -> InferKindMonad m ki a i (Kind ki a))
            -> Type ki ty a
            -> Maybe (InferKindMonad m ki a i (Kind ki a))
inferTyRecord pm pki pty pa pi inferFn ty = do
  tys <- preview _TyRecord ty
  return $ do
    let kib = review _KiBase ()
    traverse_ (traverse (\tyT -> mkCheckKind pm pki pty pa pi inferFn tyT kib)) tys
    return kib
