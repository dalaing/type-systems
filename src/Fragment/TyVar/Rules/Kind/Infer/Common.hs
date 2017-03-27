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

import Ast.Type
import Rules.Kind.Infer.Common
import Context.Type

import Fragment.KiBase.Ast.Kind

type TyVarInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , Ord a
  , AsKiBase ki
  , MonadReader r (InferKindMonad ki a m i)
  , HasTypeContext r ki a
  , MonadError e (InferKindMonad ki a m i)
  , AsUnboundTypeVariable e a
  )

tyVarInferKindInput :: TyVarInferKindConstraint e w s r m ki ty a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferKindInput e w s r m (InferKindMonad ki a m i) ki ty a i
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
           -> Maybe (InferKindMonad ki a m i (InferKind ki a i))
inferTyVar pm pki pty pa pi ty = do
  v <- preview _TyVar ty
  return $ do
    k <- lookupType v
    return . mkKind pm pki pty pa pi $ k
