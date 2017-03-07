{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.SystemF.Rules.Kind.Infer.SyntaxDirected (
    SystemFInferKindContext
  , systemFInferKindRules
  ) where

import Control.Monad (unless)

import Bound (instantiate1)
import Control.Lens (review, preview, (%~))
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Data.Functor.Classes (Eq1)

import Ast.Kind
import Ast.Type
import Ast.Type.Var
import Ast.Error.Common
import Context.Type
import Rules.Kind.Infer.SyntaxDirected

import Fragment.KiBase.Ast.Kind
import Fragment.SystemF.Ast.Type

inferTyArr :: (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTySystemF ki ty)
            => (Type ki ty a -> m (Kind ki))
            -> Type ki ty a
            -> Maybe (m (Kind ki))
inferTyArr inferFn ty = do
  (ty1, ty2) <- preview _TyArr ty
  return $ do
    let ki = review _KiBase()
    mkCheckKind inferFn ty1 ki
    mkCheckKind inferFn ty2 ki
    return . review _KiBase $ ()

inferTyAll :: (Ord a, MonadReader r m, HasTypeContext r ki a, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTySystemF ki ty)
            => (Type ki ty a -> m (Kind ki))
            -> Type ki ty a
            -> Maybe (m (Kind ki))
inferTyAll inferFn ty = do
  s <- preview _TyAll ty
  return $ do
    let ki = review _KiBase ()
    v <- freshTyVar
    ki' <- local (typeContext %~ insertType v ki) (inferFn (instantiate1 (review _TyVar v) s))
    unless (ki == ki') $
      throwing _UnexpectedKind (ExpectedKind ki, ActualKind ki')
    return ki

type SystemFInferKindContext e w s r m ki ty a = (Ord a, MonadReader r m, HasTypeContext r ki a, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTySystemF ki ty)

systemFInferKindRules :: SystemFInferKindContext e w s r m ki ty a
                      => InferKindInput e w s r m ki ty a
systemFInferKindRules =
  InferKindInput
    [ InferKindRecurse inferTyArr
    , InferKindRecurse inferTyAll
    ]
