{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.Record.Rules.Type.Infer.Common (
    RecordInferTypeConstraint
  , recordInferTypeInput
  ) where

import Control.Monad (replicateM)
import Data.Proxy (Proxy)
import GHC.Exts (Constraint)

import Bound (Bound)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import qualified Data.Text as T

import Ast.Type
import Ast.Type.Var
import Ast.Error.Common.Type
import Ast.Pattern
import Ast.Term
import Rules.Unification
import Data.Bitransversable
import Data.Functor.Rec

import Fragment.Record.Ast.Type
import Fragment.Record.Ast.Error
import Fragment.Record.Ast.Pattern
import Fragment.Record.Ast.Term

import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ITSyntax)

import Rules.Type.Infer.Offline (ITOffline)
import Control.Monad.State (MonadState)
import Data.Equivalence.Monad (classDesc)

class MkInferType i => RecordInferTypeHelper i where
  type RecordInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a i :: Constraint

  unifyRecordRules :: RecordInferTypeHelperConstraint e w s r m ki ty a i
                  => Proxy (MonadProxy e w s r m)
                  -> Proxy i
                  -> [UnificationRule m (TyAst ki ty) (TyAstVar a)]

  createRecord :: RecordInferTypeHelperConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> [(T.Text, Type ki ty a)]
             -> InferTypeMonad m ki ty a i (Type ki ty a)

  expectRecord :: RecordInferTypeHelperConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> Type ki ty a
             -> InferTypeMonad m ki ty a i [(T.Text, Type ki ty a)]


instance RecordInferTypeHelper ITSyntax where
  type RecordInferTypeHelperConstraint e w s r m ki ty a ITSyntax =
    ( AsTyRecord ki ty
    , MonadError e m
    , AsExpectedTyRecord e ki ty a
    )

  unifyRecordRules _ _  =
    []

  createRecord _ _ =
    return . review _TyRecord

  expectRecord _ _ =
    expectTyRecord

instance RecordInferTypeHelper ITOffline where
  type RecordInferTypeHelperConstraint e w s r m ki ty a ITOffline =
    ( AsTyRecord ki ty
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
    , AsExpectedTyRecord e ki ty a
    , Bound ki
    , Bound (ty ki)
    , Bitransversable ki
    , Bitransversable (ty ki)
    )

  unifyRecordRules _ _  =
    let
      unifyRecord unifyMany (UCEq ty1 ty2) = do
        let ty1' = review _Wrapped ty1
            ty2' = review _Wrapped ty2
        tys1 <- preview _TyRecord ty1'
        tys2 <- preview _TyRecord ty2'
        let tys1' = fmap (fmap (review _Unwrapped)) tys1
            tys2' = fmap (fmap (review _Unwrapped)) tys2
        return $ do
          cs1 <- traverse (classDesc . snd) tys1'
          cs2 <- traverse (classDesc . snd) tys2'
          unifyMany cs1 cs2
    in
      [ UnificationMany unifyRecord ]

  createRecord m i tys = do
    tyV <- fmap (review _TyVar) freshTyVar
    expectTypeEq m i (review _TyRecord tys) tyV
    return tyV

  expectRecord m i tyT = do
    -- this is a bit dodgy, we should probably use pattern matching
    -- rather than an ix function to access parts of a record or switch
    -- to something like row polymorphism
    tys <- expectTyRecord tyT

    tyVs <- replicateM (length tys) (fmap (review _TyVar) freshTyVar)
    let tyPs = zipWith (\x y -> (fst x, y)) tys tyVs
    expectTypeEq m i tyT (review _TyRecord tyPs)
    return tyPs


type RecordInferTypeConstraint e w s r m ki ty pt tm a i =
  ( RecordInferConstraint e w s r m ki ty pt tm a i
  , RecordCheckConstraint e w s r m ki ty pt tm a i
  )

type RecordInferConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , RecordInferTypeHelper i
  , RecordInferTypeHelperConstraint e w s r m ki ty a i
  , AsTmRecord ki ty pt tm
  , AsTyRecord ki ty
  , MonadError e (InferTypeMonad m ki ty a i)
  , AsRecordNotFound e
  )

type RecordCheckConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , RecordInferTypeHelper i
  , RecordInferTypeHelperConstraint e w s r m ki ty a i
  , AsPtRecord pt
  , AsTyRecord ki ty
  , MonadError e (InferTypeMonad m ki ty a i)
  , AsRecordNotFound e
  )

recordInferTypeInput :: RecordInferTypeConstraint e w s r m ki ty pt tm a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferTypeInput e w s r m (InferTypeMonad m ki ty a i) ki ty pt tm a
recordInferTypeInput m i =
  InferTypeInput
    (unifyRecordRules m i)
    [ InferTypeRecurse $ inferTmRecord m i
    , InferTypeRecurse $ inferTmRecordIx m i
    ]
    [ PCheckRecurse $ checkRecord m i]

inferTmRecord :: RecordInferConstraint e w s r m ki ty pt tm a i
              => Proxy (MonadProxy e w s r m)
              -> Proxy i
              -> (Term ki ty pt tm a -> InferTypeMonad m ki ty a i (Type ki ty a))
              -> Term ki ty pt tm a
              -> Maybe (InferTypeMonad m ki ty a i (Type ki ty a))
inferTmRecord m i inferFn tm = do
  tms <- preview _TmRecord tm
  return $ do
    tys <- traverse (traverse inferFn) tms
    createRecord m i tys

inferTmRecordIx :: RecordInferConstraint e w s r m ki ty pt tm a i
                => Proxy (MonadProxy e w s r m)
                -> Proxy i
                -> (Term ki ty pt tm a -> InferTypeMonad m ki ty a i (Type ki ty a))
                -> Term ki ty pt tm a
                -> Maybe (InferTypeMonad m ki ty a i (Type ki ty a))
inferTmRecordIx m i inferFn tm = do
  (tmT, ix) <- preview _TmRecordIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectRecord m i tyT
    lookupRecord tys ix

checkRecord :: RecordCheckConstraint e w s r m ki ty pt tm a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy i
            -> (Pattern pt a -> Type ki ty a -> InferTypeMonad m ki ty a i [Type ki ty a])
            -> Pattern pt a
            -> Type ki ty a
            -> Maybe (InferTypeMonad m ki ty a i [Type ki ty a])
checkRecord m i checkFn p ty = do
  ps <- preview _PtRecord p
  return $ do
    -- check for duplicate labels in ps
    ltys <- expectRecord m i ty
    let f (l, lp) = do
          typ <- lookupRecord ltys l
          checkFn lp typ
    fmap mconcat . traverse f $ ps

