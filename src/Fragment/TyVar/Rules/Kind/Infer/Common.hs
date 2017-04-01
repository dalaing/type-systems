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
{-# LANGUAGE TypeFamilies #-}
module Fragment.TyVar.Rules.Kind.Infer.Common (
    TyVarInferKindConstraint
  , tyVarInferKindInput
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens (preview)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)

import Ast.Kind
import Ast.Type
import Rules.Kind.Infer.Common
import Context.Type

import Fragment.KiBase.Ast.Kind

type TyVarInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , Ord a
  , AsKiBase ki
  , MonadReader r (InferKindMonad m ki a i)
  , HasTypeContext r ki a
  , MonadError e (InferKindMonad m ki a i)
  , AsUnboundTypeVariable e a
  )

tyVarInferKindInput :: TyVarInferKindConstraint e w s r m ki ty a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferKindInput e w s r m (InferKindMonad m ki a i) ki ty a
tyVarInferKindInput m i =
  InferKindInput
    []
    [ InferKindBase $ inferTyVar m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i ]

inferTyVar :: TyVarInferKindConstraint e w s r m ki ty a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy ki
           -> Proxy ty
           -> Proxy a
           -> Proxy i
           -> Type ki ty a
           -> Maybe (InferKindMonad m ki a i (Kind ki a))
inferTyVar _ _ _ _ _ ty = do
  v <- preview _TyVar ty
  return . lookupType $ v
