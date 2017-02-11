{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module SystemF.Check (
    TCConstraints
  , check
  , infer
  ) where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)

import Control.Lens
import Control.Monad.Error.Lens

import SystemF.Type.Class
import SystemF.Term.Class
import SystemF.Check.Classes

check' :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a)) => (tm a -> m (ty a)) -> tm a -> ty a -> m ()
check' inferFn tm ty = do
  tyAc <- inferFn tm
  expect ty tyAc

type TCConstraints r s e m ty tm a = (Ord a, Eq (ty a), MonadState s m, HasTyVarSupply s, HasTmVarSupply s, ToTyVar a, ToTmVar a, MonadReader r m, MonadError e m, AsType ty, AsTerm ty tm, HasTermContext r ty a a, AsUnknownTypeError e, AsUnboundTermVariable e a, AsUnexpected e (ty a), AsExpectedEq e (ty a), AsExpectedTyArr e (ty a), AsExpectedTyAll e (ty a))

check :: TCConstraints r s e m ty tm a => tm a -> ty a -> m ()
check = check' infer

infer :: TCConstraints r s e m ty tm a => tm a -> m (ty a)
infer tm = do
  tmV <- freshTmVar
  tyV <- freshTyVar
  fromMaybe (throwing _UnknownTypeError ()) .
    asum .
    fmap ($ tm) $ [
      inferTmVar
    , inferTmLam infer tmV
    , inferTmApp infer
    , inferTmLamTy infer tyV
    , inferTmAppTy infer
    , inferTmAdd infer
    , inferTmInt
    ]

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, AsTerm ty tm, HasTermContext r ty a a, AsUnboundTermVariable e a) => tm a -> Maybe (m (ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

inferTmLam :: (Ord a, MonadReader r m, AsType ty, AsTerm ty tm, HasTermContext r ty a a) => (tm a -> m (ty a)) -> a -> tm a -> Maybe (m (ty a))
inferTmLam inferFn v tm = do
  (_, tyArg, tmF) <- preview _TmLam (v, tm)
  return $ do
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: (Eq (ty a), MonadError e m, AsType ty, AsTerm ty tm, AsExpectedTyArr e (ty a), AsExpectedEq e (ty a)) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectEq tyArg tyX
    return tyRet

inferTmLamTy :: (Monad m, Eq a, AsType ty, AsTerm ty tm) => (tm a -> m (ty a)) -> a -> tm a -> Maybe (m (ty a))
inferTmLamTy inferFn v tm = do
  (_, tmF) <- preview _TmLamTy (v, tm)
  return $ do
    tyA <- inferFn tmF
    return . snd $ review _TyAll (v, tyA)

inferTmAppTy :: (Eq a, MonadError e m, MonadState s m, HasTyVarSupply s, ToTyVar a, AsType ty, AsTerm ty tm, AsExpectedTyAll e (ty a)) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmAppTy inferFn tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  return $ do
    tyF <- inferFn tmF
    _ <- expectTyAll tyF
    let Just ty = substTyAll tyX tyF
    return ty

inferTmAdd :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsType ty, AsTerm ty tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmAdd inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    let ty = review _TyInt ()
    check' inferFn tm1 ty
    check' inferFn tm2 ty
    return ty

inferTmInt :: (Monad m, AsType ty, AsTerm ty tm) => tm a -> Maybe (m (ty a))
inferTmInt tm = do
  _ <- preview _TmInt tm
  return . return $ review _TyInt ()
