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
module Fragment.Tuple.Rules.Kind.Infer.Common (
    TupleInferKindConstraint
  , tupleInferKindInput
  ) where

import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))

import Control.Lens (review, preview)

import Ast.Kind
import Ast.Type
import Rules.Kind.Infer.Common

import Fragment.KiBase.Ast.Kind
import Fragment.Tuple.Ast.Type

type TupleInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , AsKiBase ki
  , AsTyTuple ki ty
  )

tupleInferKindInput :: TupleInferKindConstraint e w s r m ki ty a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferKindInput e w s r m (InferKindMonad ki a m i) ki ty a
tupleInferKindInput m i =
  InferKindInput
    []
    [ InferKindRecurse $ inferTyTuple m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i ]

inferTyTuple :: TupleInferKindConstraint e w s r m ki ty a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy ki
            -> Proxy ty
            -> Proxy a
            -> Proxy i
            -> (Type ki ty a -> InferKindMonad ki a m i (Kind ki a))
            -> Type ki ty a
            -> Maybe (InferKindMonad ki a m i (Kind ki a))
inferTyTuple pm pki pty pa pi inferFn ty = do
  tys <- preview _TyTuple ty
  return $ do
    let kib = review _KiBase ()
    traverse_ (\tyT -> mkCheckKind pm pki pty pa pi inferFn tyT kib) tys
    return kib
