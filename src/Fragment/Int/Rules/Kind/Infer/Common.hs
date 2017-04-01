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
module Fragment.Int.Rules.Kind.Infer.Common (
    IntInferKindConstraint
  , intInferKindInput
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens (review, preview)

import Ast.Kind
import Ast.Type
import Rules.Kind.Infer.Common

import Fragment.KiBase.Ast.Kind
import Fragment.Int.Ast.Type

type IntInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , AsKiBase ki
  , AsTyInt ki ty
  )

intInferKindInput :: IntInferKindConstraint e w s r m ki ty a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferKindInput e w s r m (InferKindMonad m ki a i) ki ty a
intInferKindInput m i =
  InferKindInput
    []
    [ InferKindBase $ inferTyInt m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i ]

inferTyInt :: IntInferKindConstraint e w s r m ki ty a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy ki
            -> Proxy ty
            -> Proxy a
            -> Proxy i
            -> Type ki ty a
            -> Maybe (InferKindMonad m ki a i (Kind ki a))
inferTyInt pm pki pty pa pi ty = do
  _ <- preview _TyInt ty
  return . return . review _KiBase $ ()
