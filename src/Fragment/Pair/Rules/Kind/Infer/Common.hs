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
module Fragment.Pair.Rules.Kind.Infer.Common (
    PairInferKindConstraint
  , pairInferKindInput
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)
import Data.Functor.Classes (Eq1)

import Ast.Kind
import Ast.Type
import Ast.Error.Common
import Rules.Kind.Infer.Common

import Fragment.KiBase.Ast.Kind
import Fragment.Pair.Ast.Type

type PairInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , AsKiBase ki
  , AsTyPair ki ty
  )

pairInferKindInput :: PairInferKindConstraint e w s r m ki ty a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferKindInput e w s r m (InferKindMonad ki a m i) ki ty a
pairInferKindInput m i =
  InferKindInput
    []
    [ InferKindRecurse $ inferTyPair m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i ]

inferTyPair :: PairInferKindConstraint e w s r m ki ty a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy ki
            -> Proxy ty
            -> Proxy a
            -> Proxy i
            -> (Type ki ty a -> InferKindMonad ki a m i (Kind ki a))
            -> Type ki ty a
            -> Maybe (InferKindMonad ki a m i (Kind ki a))
inferTyPair pm pki pty pa pi inferFn ty = do
  (ty1, ty2) <- preview _TyPair ty
  return $ do
    let kib = review _KiBase ()
    mkCheckKind pm pki pty pa pi inferFn ty1 kib
    mkCheckKind pm pki pty pa pi inferFn ty2 kib
    return kib
