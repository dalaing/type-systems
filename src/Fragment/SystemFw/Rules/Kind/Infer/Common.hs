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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Fragment.SystemFw.Rules.Kind.Infer.Common (
    SystemFwInferKindConstraint
  , systemFwInferKindInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Bound (Scope)
import Control.Lens (preview, review, (%~))
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)

import Ast.Kind
import Ast.Kind.Var
import Ast.Type
import Ast.Type.Var
import Ast.Error.Common.Kind
import Context.Type
import Data.Functor.Rec
import Rules.Kind.Infer.Common
import Rules.Unification

import Fragment.KiArr.Ast.Kind
import Fragment.KiArr.Ast.Error

import Fragment.SystemFw.Ast.Type
import Fragment.SystemFw.Ast.Error

import Rules.Kind.Infer.SyntaxDirected (IKSyntax)
import Rules.Kind.Infer.Offline (IKOffline)

class MkInferKind i => TyLamInferKindHelper i where
  type TyLamInferKindHelperConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i :: Constraint

  expectTyLam :: TyLamInferKindHelperConstraint e w s r m ki ty a i
              => Proxy (MonadProxy e w s r m)
              -> Proxy ki
              -> Proxy ty
              -> Proxy a
              -> Proxy i
              -> Type ki ty a
              -> Maybe (InferKindMonad m ki a i (Kind ki a, Scope () (TyAst ki ty) (TyAstVar a)))

  expectArr :: TyLamInferKindHelperConstraint e w s r m ki ty a i
              => Proxy (MonadProxy e w s r m)
              -> Proxy ki
              -> Proxy ty
              -> Proxy a
              -> Proxy i
              -> Kind ki a
              -> InferKindMonad m ki a i (Kind ki a, Kind ki a)

instance TyLamInferKindHelper IKSyntax where
  type TyLamInferKindHelperConstraint e w s r m ki ty a IKSyntax =
    ( AsTySystemFw ki ty
    , AsKiArr ki
    , MonadError e m
    , AsExpectedTyLamAnnotation e
    , AsExpectedKiArr e ki a
    )

  expectTyLam _ _ _ _ _ ty = do
    (mki, s) <- preview _TyLam ty
    return $ do
      case mki of
        Nothing -> throwing _ExpectedTyLamAnnotation ()
        Just ki -> return (ki, s)

  expectArr _ _ _ _ _ =
    expectKiArr

instance TyLamInferKindHelper IKOffline where
  type TyLamInferKindHelperConstraint e w s r m ki ty a IKOffline =
    ( AsTySystemFw ki ty
    , AsKiArr ki
    , MonadState s (InferKindMonad m ki a IKOffline)
    , HasKiVarSupply s
    , ToKiVar a
    , Ord a
    , OrdRec ki
    , OrdRec (ty ki)
    , MonadError e m
    , AsUnknownKindError e
    , AsUnboundTypeVariable e a
    , AsOccursError e (Kind ki) a
    , AsUnificationMismatch e (Kind ki) a
    , AsUnificationExpectedEq e (Kind ki) a
    )

  expectTyLam pm pki pty pa pi ty = do
    (mki, s) <- preview _TyLam ty
    return $ do
      kiV <- fmap (review _KiVar) freshKiVar
      case mki of
        Nothing -> return ()
        Just ki -> expectKind pm pki pty pa pi (ExpectedKind ki) (ActualKind kiV)
      return (kiV, s)

  expectArr pm pki pty pa pi kiA = do
    kiP1 <- fmap (review _KiVar) freshKiVar
    kiP2 <- fmap (review _KiVar) freshKiVar
    expectKindEq pm pki pty pa pi kiA (review _KiArr (kiP1, kiP2))
    return (kiP1, kiP2)

type SystemFwInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , AsTySystemFw ki ty
  , AsKiArr ki
  , MonadReader r (InferKindMonad m ki a i)
  , HasTypeContext r ki a
  , MonadState s (InferKindMonad m ki a i)
  , HasTyVarSupply s
  , ToTyVar a
  , Ord a
  , TyLamInferKindHelper i
  , TyLamInferKindHelperConstraint e w s r m ki ty a i
  )

inferTyLam :: SystemFwInferKindConstraint e w s r m ki ty a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy ki
           -> Proxy ty
           -> Proxy a
           -> Proxy i
           -> (Type ki ty a -> InferKindMonad m ki a i (Kind ki a))
           -> Type ki ty a
           -> Maybe (InferKindMonad m ki a i (Kind ki a))
inferTyLam pm pki pty pa pi inferFn ty = do
  act <- expectTyLam pm pki pty pa pi ty
  return $ do
    (kiArg :: Kind ki a, s) <- act
    v <- freshTyVar
    let tyF = instantiateTy (review _TyVar v) s
    kiRet <- local (typeContext %~ insertType v kiArg) $ inferFn tyF
    return $ review _KiArr (kiArg, kiRet)

inferTyApp :: SystemFwInferKindConstraint e w s r m ki ty a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy ki
           -> Proxy ty
           -> Proxy a
           -> Proxy i
           -> (Type ki ty a -> InferKindMonad m ki a i (Kind ki a))
           -> Type ki ty a
           -> Maybe (InferKindMonad m ki a i (Kind ki a))
inferTyApp pm pki pty pa pi inferFn ty = do
  (tyF, tyX) <- preview _TyApp ty
  return $ do
    kiF <- inferFn tyF
    (kiArg, kiRet) <- expectArr pm pki pty pa pi kiF
    kiX <- inferFn tyX
    expectKindEq pm pki pty pa pi kiArg kiX
    return kiRet

systemFwInferKindInput :: SystemFwInferKindConstraint e w s r m ki ty a i
                       => Proxy (MonadProxy e w s r m)
                       -> Proxy i
                       -> InferKindInput e w s r m (InferKindMonad m ki a i) ki ty a
systemFwInferKindInput m i =
  InferKindInput
    []
    [ InferKindRecurse $ inferTyLam m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i
    , InferKindRecurse $ inferTyApp m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i
    ]
