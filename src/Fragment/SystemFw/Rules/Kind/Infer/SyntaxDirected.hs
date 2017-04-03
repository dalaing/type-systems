{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.SystemFw.Rules.Kind.Infer.SyntaxDirected (
    SystemFwInferKindContext
  , systemFwInferKindRules
  ) where

import Control.Monad (unless)

import Bound (Bound, instantiate1)
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
import Fragment.SystemFw.Ast.Kind
import Fragment.SystemFw.Ast.Type
import Fragment.SystemFw.Ast.Error

inferTyLam :: (Ord a, Bound (ty ki), MonadState s m, HasTyVarSupply s, ToTyVar a, MonadReader r m, AsKiSystemFw ki, AsTySystemFw ki ty, HasTypeContext r ki a)
           => (Type ki ty a -> m (Kind ki))
           -> Type ki ty a
           -> Maybe (m (Kind ki))
inferTyLam inferFn ty = do
  (kiArg, s) <- preview _TyLam ty
  return $ do
    v <- freshTyVar
    let tyF = instantiate1 (review _TyVar v) s
    kiRet <- local (typeContext %~ insertType v kiArg) $ inferFn tyF
    return $ review _KiArr (kiArg, kiRet)

inferTyApp :: (Eq a, Eq1 ki, MonadError e m, AsKiSystemFw ki, AsTySystemFw ki ty, AsExpectedKiArr e ki, AsExpectedKindEq e ki)
           => (Type ki ty a -> m (Kind ki))
           -> Type ki ty a
           -> Maybe (m (Kind ki))
inferTyApp inferFn ty = do
  (tyF, tyX) <- preview _TyApp ty
  return $ do
    kiF <- inferFn tyF
    (kiArg, kiRet) <- expectKiArr kiF
    kiX <- inferFn tyX
    expectKindEq kiArg kiX
    return kiRet

type SystemFwInferKindContext e w s r m ki ty a = (Ord a, Eq1 ki, Bound (ty ki), MonadReader r m, HasTypeContext r ki a, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsExpectedKindEq e ki, AsUnexpectedKind e ki, AsExpectedKiArr e ki, Eq1 ki, AsKiBase ki, AsKiSystemFw ki, AsTySystemFw ki ty)

systemFwInferKindRules :: SystemFwInferKindContext e w s r m ki ty a
                       => InferKindInput e w s r m ki ty a
systemFwInferKindRules =
  InferKindInput
    [ InferKindRecurse inferTyArr
    , InferKindRecurse inferTyAll
    , InferKindRecurse inferTyLam
    , InferKindRecurse inferTyApp
    ]
