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
module Fragment.Tuple.Rules.Type.Infer.Common (
    TupleInferTypeHelper(..)
  , TupleInferTypeConstraint
  , tupleInferTypeInput
  ) where

import Control.Monad (zipWithM, replicateM)
import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Bound (Bound)
import Control.Lens (preview, review)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Monad.Except (MonadError)

import Ast.Type
import Ast.Type.Var
import Ast.Error.Common.Type
import Ast.Pattern
import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec

import Fragment.Tuple.Ast.Type
import Fragment.Tuple.Ast.Error
import Fragment.Tuple.Ast.Pattern
import Fragment.Tuple.Ast.Term

import Rules.Unification
import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ITSyntax)

import Rules.Type.Infer.Offline (ITOffline)
import Control.Monad.State (MonadState)
import Data.Equivalence.Monad (classDesc)

class MkInferType i => TupleInferTypeHelper i where
  type TupleInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i :: Constraint

  unifyTupleRules :: TupleInferTypeHelperConstraint e w s r m ki ty a i
                  => Proxy (MonadProxy e w s r m)
                  -> Proxy i
                  -> [UnificationRule m (TyAst ki ty) (TyAstVar a)]

  createTuple :: TupleInferTypeHelperConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> [Type ki ty a]
             -> InferTypeMonad ki ty a m i (Type ki ty a)

  expectTuple :: TupleInferTypeHelperConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> Type ki ty a
             -> InferTypeMonad ki ty a m i [Type ki ty a]


instance TupleInferTypeHelper ITSyntax where
  type TupleInferTypeHelperConstraint e w s r m ki ty a ITSyntax =
    ( AsTyTuple ki ty
    , MonadError e m
    , AsExpectedTyTuple e ki ty a
    )

  unifyTupleRules _ _  =
    []

  createTuple _ _ =
    return . review _TyTuple

  expectTuple _ _ =
    expectTyTuple

instance TupleInferTypeHelper ITOffline where
  type TupleInferTypeHelperConstraint e w s r m ki ty a ITOffline =
    ( AsTyTuple ki ty
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
    , AsExpectedTyTuple e ki ty a
    , Bound ki
    , Bound (ty ki)
    , Bitransversable ki
    , Bitransversable (ty ki)
    )

  unifyTupleRules _ _  =
    let
      unifyTuple unifyMany (UCEq ty1 ty2) = do
        let ty1' = review _Wrapped ty1
            ty2' = review _Wrapped ty2
        tys1 <- preview _TyTuple ty1'
        tys2 <- preview _TyTuple ty2'
        let tys1' = fmap (review _Unwrapped) tys1
            tys2' = fmap (review _Unwrapped) tys2
        return $ do
          cs1 <- traverse classDesc tys1'
          cs2 <- traverse classDesc tys2'
          unifyMany cs1 cs2
    in
      [ UnificationMany unifyTuple ]

  createTuple m i tys = do
    tyV <- fmap (review _TyVar) freshTyVar
    expectTypeEq m i (review _TyTuple tys) tyV
    return tyV

  expectTuple m i tyT = do
    -- this is a bit dodgy, we should probably use pattern matching
    -- rather than an ix function to access parts of a tuple or switch
    -- to something like row polymorphism
    tys <- expectTyTuple tyT

    tyVs <- replicateM (length tys) (fmap (review _TyVar) freshTyVar)
    expectTypeEq m i tyT (review _TyTuple tyVs)
    return tyVs


type TupleInferTypeConstraint e w s r m ki ty pt tm a i =
  ( TupleInferConstraint e w s r m ki ty pt tm a i
  , TupleCheckConstraint e w s r m ki ty pt tm a i
  )

type TupleInferConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , TupleInferTypeHelper i
  , TupleInferTypeHelperConstraint e w s r m ki ty a i
  , AsTmTuple ki ty pt tm
  , AsTyTuple ki ty
  , MonadError e (InferTypeMonad ki ty a m i)
  , AsTupleOutOfBounds e
  )

type TupleCheckConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , TupleInferTypeHelper i
  , TupleInferTypeHelperConstraint e w s r m ki ty a i
  , AsPtTuple pt
  , AsTyTuple ki ty
  )

tupleInferTypeInput :: TupleInferTypeConstraint e w s r m ki ty pt tm a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
tupleInferTypeInput m i =
  InferTypeInput
    (unifyTupleRules m i)
    [ InferTypeRecurse $ inferTmTuple m i
    , InferTypeRecurse $ inferTmTupleIx m i
    ]
    [ PCheckRecurse $ checkTuple m i]

inferTmTuple :: TupleInferConstraint e w s r m ki ty pt tm a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy i
            -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
            -> Term ki ty pt tm a
            -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmTuple m i inferFn tm = do
  tms <- preview _TmTuple tm
  return $ do
    tys <- traverse inferFn tms
    createTuple m i tys

inferTmTupleIx :: TupleInferConstraint e w s r m ki ty pt tm a i
          => Proxy (MonadProxy e w s r m)
          -> Proxy i
          -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
          -> Term ki ty pt tm a
          -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmTupleIx m i inferFn tm = do
  (tmT, ix) <- preview _TmTupleIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectTuple m i tyT
    lookupTuple tys ix

checkTuple :: TupleCheckConstraint e w s r m ki ty pt tm a i
          => Proxy (MonadProxy e w s r m)
          -> Proxy i
          -> (Pattern pt a -> Type ki ty a -> InferTypeMonad ki ty a m i [Type ki ty a])
          -> Pattern pt a
          -> Type ki ty a
          -> Maybe (InferTypeMonad ki ty a m i [Type ki ty a])
checkTuple m i checkFn p ty = do
  pts <- preview _PtTuple p
  return $ do
    tys <- expectTuple m i ty
    ms <- zipWithM checkFn pts tys
    return $ mconcat ms
