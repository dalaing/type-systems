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
module Fragment.Pair.Rules.Type.Infer.Common (
    PairInferTypeHelper(..)
  , PairInferTypeConstraint
  , pairInferTypeInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Bound (Bound)
import Control.Lens (preview, review)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import Ast.Type
import Ast.Type.Var
import Ast.Error.Common.Type
import Ast.Pattern
import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec
import Rules.Unification

import Fragment.Pair.Ast.Type
import Fragment.Pair.Ast.Error
import Fragment.Pair.Ast.Pattern
import Fragment.Pair.Ast.Term

import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ITSyntax)
import Control.Monad.Except (MonadError)

import Rules.Type.Infer.Offline (ITOffline)
import Control.Monad.State (MonadState)
import Data.Equivalence.Monad (classDesc)

class MkInferType i => PairInferTypeHelper i where
  type PairInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i :: Constraint

  unifyPairRules :: PairInferTypeHelperConstraint e w s r m ki ty a i
                 => Proxy (MonadProxy e w s r m)
                 -> Proxy i
                 -> [UnificationRule m (TyAst ki ty) (TyAstVar a)]

  createPair :: PairInferTypeHelperConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> Type ki ty a
             -> Type ki ty a
             -> InferTypeMonad m ki ty a i (Type ki ty a)

  expectPair :: PairInferTypeHelperConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> Type ki ty a
             -> InferTypeMonad m ki ty a i (Type ki ty a, Type ki ty a)

instance PairInferTypeHelper ITSyntax where
  type PairInferTypeHelperConstraint e w s r m ki ty a ITSyntax =
    ( AsTyPair ki ty
    , MonadError e m
    , AsExpectedTyPair e ki ty a
    )

  unifyPairRules _ _  =
    []

  createPair _ _ ty1 ty2 =
    return . review _TyPair $ (ty1, ty2)

  expectPair _ _ =
    expectTyPair

instance PairInferTypeHelper ITOffline where
  type PairInferTypeHelperConstraint e w s r m ki ty a ITOffline =
    ( AsTyPair ki ty
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

  unifyPairRules _ _  =
    let
      unifyPair unifyMany (UCEq ty1 ty2) = do
        let ty1' = review _Wrapped ty1
            ty2' = review _Wrapped ty2
        (p1a, p1b) <- preview _TyPair ty1'
        (p2a, p2b) <- preview _TyPair ty2'
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
      [ UnificationMany unifyPair ]

  createPair m i ty1 ty2 = do
    tyV <- fmap (review _TyVar) freshTyVar
    expectTypeEq m i (review _TyPair (ty1, ty2)) tyV
    return tyV

  expectPair m i tyP = do
    tyP1 <- fmap (review _TyVar) freshTyVar
    tyP2 <- fmap (review _TyVar) freshTyVar
    expectTypeEq m i tyP (review _TyPair (tyP1, tyP2))
    return (tyP1, tyP2)

type PairInferTypeConstraint e w s r m ki ty pt tm a i =
  ( PairInferConstraint e w s r m ki ty pt tm a i
  , PairCheckConstraint e w s r m ki ty pt tm a i
  )

type PairInferConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , PairInferTypeHelper i
  , PairInferTypeHelperConstraint e w s r m ki ty a i
  , AsTmPair ki ty pt tm
  , AsTyPair ki ty
  )

type PairCheckConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , PairInferTypeHelper i
  , PairInferTypeHelperConstraint e w s r m ki ty a i
  , AsPtPair pt
  , AsTyPair ki ty
  )

pairInferTypeInput :: PairInferTypeConstraint e w s r m ki ty pt tm a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferTypeInput e w s r m (InferTypeMonad m ki ty a i) ki ty pt tm a
pairInferTypeInput m i =
  InferTypeInput
    (unifyPairRules m i)
    [ InferTypeRecurse $ inferTmPair m i
    , InferTypeRecurse $ inferTmFst m i
    , InferTypeRecurse $ inferTmSnd m i
    ]
    [ PCheckRecurse $ checkPair m i]


inferTmPair :: PairInferConstraint e w s r m ki ty pt tm a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy i
            -> (Term ki ty pt tm a -> InferTypeMonad m ki ty a i (Type ki ty a))
            -> Term ki ty pt tm a
            -> Maybe (InferTypeMonad m ki ty a i (Type ki ty a))
inferTmPair m i inferFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    createPair m i ty1 ty2

inferTmFst :: PairInferConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad m ki ty a i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad m ki ty a i (Type ki ty a))
inferTmFst m i inferFn tm = do
  tmP <- preview _TmFst tm
  return $ do
    tyP <- inferFn tmP
    (ty1, _) <- expectPair m i tyP
    return ty1

inferTmSnd :: PairInferConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad m ki ty a i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad m ki ty a i (Type ki ty a))
inferTmSnd m i inferFn tm = do
  tmP <- preview _TmFst tm
  return $ do
    tyP <- inferFn tmP
    (_, ty2) <- expectPair m i tyP
    return ty2

checkPair :: PairCheckConstraint e w s r m ki ty pt tm a i
          => Proxy (MonadProxy e w s r m)
          -> Proxy i
          -> (Pattern pt a -> Type ki ty a -> InferTypeMonad m ki ty a i [Type ki ty a])
          -> Pattern pt a
          -> Type ki ty a
          -> Maybe (InferTypeMonad m ki ty a i [Type ki ty a])
checkPair m i checkFn p ty = do
  (p1, p2) <- preview _PtPair p
  return $ do
    (ty1, ty2) <- expectPair m i ty
    mappend <$> checkFn p1 ty1 <*> checkFn p2 ty2
