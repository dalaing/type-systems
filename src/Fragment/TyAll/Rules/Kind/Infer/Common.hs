{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TyAll.Rules.Kind.Infer.Common (
    TyAllInferKindConstraint
  , TyAllInferKindHelper(..)
  , tyAllInferKindInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Bound (Scope, instantiate1)
import Control.Lens (preview, review, (%~))
import Control.Lens.Wrapped (_Wrapped, )
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Ast.Kind
import Ast.Kind.Var
import Ast.Type
import Ast.Type.Var
import Ast.Error.Common.Kind
import Data.Functor.Rec
import Context.Type

import Fragment.KiArr.Ast.Kind
import Fragment.TyAll.Ast.Type
import Fragment.TyAll.Ast.Error

import Rules.Kind.Infer.Common

import Rules.Kind.Infer.SyntaxDirected (IKSyntax)
import Rules.Kind.Infer.Offline (IKOffline)
import Rules.Unification

class MkInferKind i => TyAllInferKindHelper i where
  type TyAllInferKindHelperConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i :: Constraint

  expectAll :: TyAllInferKindHelperConstraint e w s r m ki ty a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy ki
            -> Proxy ty
            -> Proxy a
            -> Proxy i
            -> Type ki ty a
            -> Maybe (InferKindMonad ki a m i (Kind ki a, Scope () (TyAst ki ty) (TyAstVar a)))

instance TyAllInferKindHelper IKSyntax where
  type TyAllInferKindHelperConstraint e w s r m ki ty a IKSyntax =
    ( AsTyAll ki ty
    , MonadError e m
    , AsExpectedTyAllAnnotation e
    )

  expectAll _ _ _ _ _ ty = do
    (mki, s) <- preview _TyAll ty
    return $ do
      case mki of
        Nothing -> throwing _ExpectedTyAllAnnotation ()
        Just ki -> return (ki, s)

instance TyAllInferKindHelper IKOffline where
  type TyAllInferKindHelperConstraint e w s r m ki ty a IKOffline =
    ( AsTyAll ki ty
    , MonadState s m
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

  expectAll pm pki pty pa pi ty = do
    (mki, s) <- preview _TyAll ty
    return $ do
      kiV <- fmap (review _KiVar) freshKiVar
      case mki of
        Nothing -> return ()
        Just ki -> expectKind pm pki pty pa pi (ExpectedKind ki) (ActualKind kiV)
      return (kiV, s)

type TyAllInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , TyAllInferKindHelper i
  , TyAllInferKindHelperConstraint e w s r m ki ty a i
  , AsTyAll ki ty
  , AsKiArr ki
  , Ord a
  , MonadReader r (InferKindMonad ki a m i)
  , HasTypeContext r ki a
  , MonadState s (InferKindMonad ki a m i)
  , HasKiVarSupply s
  , ToKiVar a
  , HasTyVarSupply s
  , ToTyVar a
  )

inferTyAll :: TyAllInferKindConstraint e w s r m ki ty a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy ki
           -> Proxy ty
           -> Proxy a
           -> Proxy i
           -> (Type ki ty a -> InferKindMonad ki a m i (Kind ki a))
           -> Type ki ty a
           -> Maybe (InferKindMonad ki a m i (Kind ki a))
inferTyAll pm pki pty pa pi inferFn ty = do
  act <- expectAll pm pki pty pa pi ty
  return $ do
    (kiArg, s) <- act
    v <- freshTyVar
    let tyF = review _Wrapped $ instantiate1 (review (_TyAstVar . _TyAstTyVar) v) s
    kiRet <- local (typeContext %~ insertType v kiArg) $ inferFn tyF
    return . review _KiArr $ (kiArg, kiRet)

tyAllInferKindInput :: TyAllInferKindConstraint e w s r m ki ty a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferKindInput e w s r m (InferKindMonad ki a m i) ki ty a
tyAllInferKindInput m i =
  InferKindInput
    []
    [InferKindRecurse $ inferTyAll m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i]
