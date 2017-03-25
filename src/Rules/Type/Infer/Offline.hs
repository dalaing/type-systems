{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Rules.Type.Infer.Offline (
    InferTypeRule(..)
  , PCheckRule(..)
  , UnifyT
  , InferTypeInput(..)
  , InferTypeOutput(..)
  , ITOffline
  ) where

import Control.Monad (unless)
import Data.List (tails)
import Data.Proxy (Proxy(..))

import Bound (Bound)
import Control.Monad.Except (MonadError)
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT, runWriterT)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N

import qualified Data.Map as M

import Data.Bitransversable
import Data.Functor.Rec

import Ast.Type
import Ast.Term
import Ast.Error.Common

import Rules.Unification

import Rules.Type.Infer.Common

type UnifyT ki ty a = WriterT [UConstraint (Type ki ty) a]

mkInferType' :: (UnificationContext e m (Type ki ty) a, MonadError e m, AsUnknownTypeError e)
        => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
        -> ([UConstraint (Type ki ty) a] -> m (M.Map a (Type ki ty a)))
        -> Term ki ty pt tm a
        -> m (Type ki ty a)
mkInferType' go unifyFn x = do
  (ty, cs) <- runWriterT $ go x
  s <- unifyFn cs
  return $ mapSubst _TyVar s ty

data ITOffline

mkCheck' :: MkInferTypeConstraint e w s r m ki ty a ITOffline
        => Proxy (MonadProxy e w s r m)
        -> (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
        -> ([UConstraint (Type ki ty) a] -> m (M.Map a (Type ki ty a)))
        -> Term ki ty pt tm a
        -> Type ki ty a
        -> m ()
mkCheck' m inferFn unifyFn x y = do
  cs <- execWriterT $ (mkCheckType m (Proxy :: Proxy ITOffline) inferFn) x y
  _ <- unifyFn cs
  return ()

instance MkInferType ITOffline where
  type MkInferTypeConstraint e w s r m ki ty a ITOffline =
    ( Ord a
    , OrdRec (ty ki)
    , MonadError e m
    , AsUnknownTypeError e
    , AsOccursError e (Type ki ty) a
    , AsUnificationMismatch e (Type ki ty) a
    , AsUnificationExpectedEq e (Type ki ty) a
    , Bound (ty ki)
    , Bitransversable (ty ki)
    )
  type InferTypeMonad ki ty a m ITOffline =
    UnifyT ki ty a m
  type MkInferErrorList ki ty pt tm a ITOffline =
    '[ ErrOccursError (Type ki ty) a
     , ErrUnificationMismatch (Type ki ty) a
     , ErrUnificationExpectedEq (Type ki ty) a
     ]
  type MkInferWarningList ki ty pt tm a ITOffline =
    '[]

  mkCheckType m i =
    mkCheckType' (expectType m i)

  expectType _ _ (ExpectedType ty1) (ActualType ty2) =
    unless (ty1 == ty2) $
      tell [UCEq ty1 ty2]

  expectTypeEq _ _ ty1 ty2 =
    unless (ty1 == ty2) $
      tell [UCEq ty1 ty2]

  expectTypeAllEq _ _ n@(ty :| tys) = do
    unless (all (== ty) tys ) $
      let
        xss = tails . N.toList $ n
        f [] = []
        f (x : xs) = fmap (UCEq x) xs
        ws = xss >>= f
      in
        tell ws
    return ty

  prepareInferType pm pi inferKindFn normalizeFn ii =
    let
      u = mkUnify _TyVar normalizeFn . iiUnifyRules $ ii
      pc = mkPCheck . iiPCheckRules $ ii
      i = mkInferType inferKindFn normalizeFn pc . iiInferTypeRules $ ii
      i' = mkInferType' i u
      c = mkCheck' pm i u
    in
      InferTypeOutput u i' c
