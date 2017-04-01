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
module Fragment.TyArr.Rules.Type.Infer.Common (
    TyArrInferTypeHelper(..)
  , TyArrInferTypeConstraint
  , tyArrInferTypeInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Bound (Bound)
import Control.Lens (preview, review)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Data.Equivalence.Monad (classDesc)

import Ast.Type
import Ast.Type.Var
import Ast.Error.Common.Type
import Data.Bitransversable
import Data.Functor.Rec
import Rules.Unification

import Fragment.TyArr.Ast.Type
import Fragment.TyArr.Ast.Error

import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ITSyntax)
import Rules.Type.Infer.Offline (ITOffline)

class MkInferType i => TyArrInferTypeHelper i where
  type TyArrInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i :: Constraint

  unifyTyArrRules :: TyArrInferTypeHelperConstraint e w s r m ki ty a i
                 => Proxy (MonadProxy e w s r m)
                 -> Proxy i
                 -> [UnificationRule m (TyAst ki ty) (TyAstVar a)]

  expectArr :: TyArrInferTypeHelperConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> Type ki ty a
             -> InferTypeMonad m ki ty a i (Type ki ty a, Type ki ty a)


instance TyArrInferTypeHelper ITSyntax where
  type TyArrInferTypeHelperConstraint e w s r m ki ty a ITSyntax =
    ( AsTyArr ki ty
    , MonadError e m
    , AsExpectedTyArr e ki ty a
    )

  unifyTyArrRules _ _  =
    []

  expectArr _ _ =
    expectTyArr

instance TyArrInferTypeHelper ITOffline where
  type TyArrInferTypeHelperConstraint e w s r m ki ty a ITOffline =
    ( AsTyArr ki ty
    , MonadState s m
    , HasTyVarSupply s
    , ToTyVar a
    , Ord a
    , OrdRec ki
    , OrdRec (ty ki)
    , MonadError e m
    , AsUnknownTypeError e
    , AsOccursError e (TyAst ki ty) (TyAstVar a)
    , AsUnificationMismatch e (TyAst ki ty) (TyAstVar a)
    , AsUnificationExpectedEq e (TyAst ki ty) (TyAstVar a)
    , Bound ki
    , Bound (ty ki)
    , Bitransversable ki
    , Bitransversable (ty ki)
    )

  unifyTyArrRules _ _  =
    let
      unifyTyArr unifyMany (UCEq ty1 ty2) = do
        let ty1' = review _Wrapped ty1
            ty2' = review _Wrapped ty2
        (p1a, p1b) <- preview _TyArr ty1'
        (p2a, p2b) <- preview _TyArr ty2'
        let p1a' = review _Unwrapped p1a
            p1b' = review _Unwrapped p1b
            p2a' = review _Unwrapped p2a
            p2b' = review _Unwrapped p2b
        return $ do
          c1a <- classDesc p1a'
          c1b <- classDesc p1b'
          c2a <- classDesc p2a'
          c2b <- classDesc p2b'
          unifyMany [c1a, c1b] [c2a, c2b]
    in
      [ UnificationMany unifyTyArr ]

  expectArr m i tyA = do
    tyP1 <- fmap (review _TyVar) freshTyVar
    tyP2 <- fmap (review _TyVar) freshTyVar
    expectTypeEq m i tyA (review _TyArr (tyP1, tyP2))
    return (tyP1, tyP2)

type TyArrInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , TyArrInferTypeHelper i
  , TyArrInferTypeHelperConstraint e w s r m ki ty a i
  )

tyArrInferTypeInput :: TyArrInferTypeConstraint e w s r m ki ty pt tm a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferTypeInput e w s r m (InferTypeMonad m ki ty a i) ki ty pt tm a
tyArrInferTypeInput m i =
  InferTypeInput (unifyTyArrRules m i) [] []
