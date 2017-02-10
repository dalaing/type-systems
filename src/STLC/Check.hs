{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module STLC.Check (
    check
  , runCheck
  ) where

import Control.Monad (unless)

import Control.Monad.Reader (MonadReader, runReaderT, local)
import Control.Monad.Except (MonadError, runExcept)

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Bound

import STLC.Type
import STLC.Term

data TermContext tm ty = TermContext (M.Map tm (Type ty))

emptyTermContext :: TermContext tm ty
emptyTermContext = TermContext M.empty

class HasTermContext l tm ty | l -> tm, l -> ty where
  termContext :: Lens' l (TermContext tm ty)

instance HasTermContext (TermContext tm ty) tm ty where
  termContext = id

class AsUnboundVariable e tm | e -> tm where
  _UnboundVariable :: Prism' e tm

lookupTerm :: (Ord tm, MonadReader r m, MonadError e m, HasTermContext r tm ty, AsUnboundVariable e tm) => tm -> m (Type ty)
lookupTerm v = do
  TermContext m <- view termContext
  case M.lookup v m of
    Nothing -> throwing _UnboundVariable v
    Just ty -> return ty

insertTerm :: Ord tm => tm -> Type ty -> TermContext tm ty -> TermContext tm ty
insertTerm tm ty (TermContext m) = TermContext (M.insert tm ty m)

class AsUnexpected e ty | e -> ty where
  _Unexpected :: Prism' e (Type ty, Type ty)

expect :: (Eq ty, MonadError e m, AsUnexpected e ty) => Type ty -> Type ty -> m ()
expect ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _Unexpected (ty1, ty2)

class AsExpectedEq e ty | e -> ty where
  _ExpectedEq :: Prism' e (Type ty, Type ty)

expectEq :: (Eq ty, MonadError e m, AsExpectedEq e ty) => Type ty -> Type ty -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _ExpectedEq (ty1, ty2)

class AsExpectedFnTm e tm | e -> tm where
  _ExpectedFnTm :: Prism' e (Term tm)

expectFnTm :: (MonadError e m, AsExpectedFnTm e tm) => Term tm -> m (Type ty)
expectFnTm tm =
  case tm of
    TmLam _ tyArg _ -> let Just tyArg' = closed tyArg in return tyArg'
    _ -> throwing _ExpectedFnTm tm

class AsExpectedFnTy e ty | e -> ty where
  _ExpectedFnTy :: Prism' e (Type ty)

expectFnTy :: (MonadError e m, AsExpectedFnTy e ty) => Type ty -> m (Type ty, Type ty)
expectFnTy ty =
  case ty of
    TyArr tyArg tyRet -> return (tyArg, tyRet)
    _ -> throwing _ExpectedFnTy ty

type AsSimpleSTLCErrors e tm ty = (Ord tm, Eq ty, AsUnboundVariable e tm, AsUnexpected e ty, AsExpectedEq e ty, AsExpectedFnTy e ty, AsExpectedFnTm e tm)

runCheck :: AsSimpleSTLCErrors e T.Text ty => Term T.Text -> Type ty -> Either e ()
runCheck tm ty = runExcept (runReaderT (check tm ty) emptyTermContext)

check :: (MonadReader r m, MonadError e m, HasTermContext r T.Text ty, AsSimpleSTLCErrors e T.Text ty) => Term T.Text -> Type ty -> m ()
check (TmVar a) ty = do
  ty' <- lookupTerm a
  expect ty' ty
check (TmLam v tyL s) ty = do
  let Just tyL' = closed tyL
  (tyArg, tyRet) <- expectFnTy ty
  expectEq tyL' tyArg
  local (termContext %~ insertTerm v tyArg) $ check (instantiate1 (TmVar v) s) tyRet
check (TmApp l x) tyRet = do
  tyArg <- expectFnTm l
  check l (TyArr tyArg tyRet)
  check x tyArg
check (TmInt _) ty =
  expect TyInt ty
check (TmBool _) ty =
  expect TyBool ty
check (TmAdd x y) ty = do
  check x TyInt
  check y TyInt
  expect TyInt ty
check (TmAnd x y) ty = do
  check x TyBool
  check y TyBool
  expect TyBool ty
check (TmEq x y) ty = do
  check x TyInt
  check y TyInt
  expect TyBool ty
check (TmLt x y) ty = do
  check x TyInt
  check y TyInt
  expect TyBool ty
check (TmIf b t e) ty = do
  check b TyBool
  check t ty
  check e ty
check (TmAnn tyA tm) ty = do
  let Just tyA' = closed tyA
  check tm tyA'
  expectEq tyA' ty
