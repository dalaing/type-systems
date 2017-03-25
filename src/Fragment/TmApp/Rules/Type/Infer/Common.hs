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
{-# LANGUAGE TypeFamilies #-}
module Fragment.TmApp.Rules.Type.Infer.Common (
    TmAppInferTypeConstraint
  , TmAppInferTypeHelper(..)
  , tmAppInferTypeInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Bound (Bound)
import Control.Lens (preview, review)

import Ast.Type
import Ast.Type.Var
import Ast.Error.Common.Type
import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec

import Fragment.TyArr.Ast.Type
import Fragment.TyArr.Ast.Error
import Fragment.TmApp.Ast.Term

import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ITSyntax)
import Control.Monad.Except (MonadError)

import Rules.Type.Infer.Offline (ITOffline)
import Rules.Unification
import Control.Monad.State (MonadState)

class MkInferType i => TmAppInferTypeHelper i where
  type TmAppInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a i :: Constraint

  expectArr :: TmAppInferTypeHelperConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> Type ki ty a
             -> InferTypeMonad ki ty a m i (Type ki ty a, Type ki ty a)

instance TmAppInferTypeHelper ITSyntax where
  type TmAppInferTypeHelperConstraint e w s r m ki ty a ITSyntax =
    ( AsTyArr ki ty
    , MonadError e m
    , AsExpectedTyArr e ki ty a
    )

  expectArr _ _ =
    expectTyArr

instance TmAppInferTypeHelper ITOffline where
  type TmAppInferTypeHelperConstraint e w s r m ki ty a ITOffline =
    ( AsTyArr ki ty
    , MonadState s m
    , HasTyVarSupply s
    , ToTyVar a
    , Ord a
    , OrdRec (ty ki)
    , MonadError e m
    , AsUnknownTypeError e
    , AsOccursError e (Type ki ty) a
    , AsUnificationMismatch e (Type ki ty) a
    , AsUnificationExpectedEq e (Type ki ty) a
    , Bound (ty ki)
    , Bitransversable (ty ki)
    )

  expectArr m i tyA = do
    tyP1 <- fmap (review _TyVar) freshTyVar
    tyP2 <- fmap (review _TyVar) freshTyVar
    expectTypeEq m i tyA (review _TyArr (tyP1, tyP2))
    return (tyP1, tyP2)

type TmAppInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , TmAppInferTypeHelper i
  , TmAppInferTypeHelperConstraint e w s r m ki ty a i
  , AsTmApp ki ty pt tm
  , AsTyArr ki ty
  )

inferTmApp :: TmAppInferTypeConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmApp m i inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectArr m i tyF
    tyX <- inferFn tmX
    expectTypeEq m i tyArg tyX
    return tyRet

tmAppInferTypeInput :: TmAppInferTypeConstraint e w s r m ki ty pt tm a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
tmAppInferTypeInput m i =
  InferTypeInput
    []
    [InferTypeRecurse $ inferTmApp m i]
    []

