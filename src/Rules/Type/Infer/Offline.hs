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
module Rules.Type.Infer.Offline (
    InferTypeRule(..)
  , PCheckRule(..)
  , mkCheckType
  , UnifyT
  , expectType
  , expectTypeEq
  , expectTypeAllEq
  , InferTypeInput(..)
  , InferTypeOutput(..)
  , InferTypeContext
  , prepareInferType
  ) where

import Control.Monad (unless)
import Data.List (tails)

import Bound (Bound)
import Control.Monad.Except (MonadError)
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT, runWriterT)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N

import qualified Data.Map as M

import Data.Bitransversable
import Data.Functor.Rec

import Ast.Kind
import Ast.Type
import Ast.Term
import Ast.Error.Common

import Rules.Unification

import Rules.Type.Infer.Common

type UnifyT ki ty a = WriterT [UConstraint (Type ki ty) a]

mkCheckType :: (Eq a, EqRec (ty ki), Monad m)
            => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
            -> Term ki ty pt tm a
            -> Type ki ty a
            -> UnifyT ki ty a m ()
mkCheckType = mkCheckType' expectType

mkInferType' :: (UnificationContext e m (Type ki ty) a, MonadError e m, AsUnknownTypeError e)
        => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
        -> ([UConstraint (Type ki ty) a] -> m (M.Map a (Type ki ty a)))
        -> Term ki ty pt tm a
        -> m (Type ki ty a)
mkInferType' go unifyFn x = do
  (ty, cs) <- runWriterT $ go x
  s <- unifyFn cs
  return $ mapSubst _TyVar s ty

mkCheck' :: (Eq a, EqRec (ty ki), Monad m)
        => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
        -> ([UConstraint (Type ki ty) a] -> m (M.Map a (Type ki ty a)))
        -> Term ki ty pt tm a
        -> Type ki ty a
        -> m ()
mkCheck' inferFn unifyFn x y = do
  cs <- execWriterT $ (mkCheckType inferFn) x y
  _ <- unifyFn cs
  return ()

expectType :: (Eq a, EqRec (ty ki), Monad m) => ExpectedType ki ty a -> ActualType ki ty a -> UnifyT ki ty a m ()
expectType (ExpectedType ty1) (ActualType ty2) =
  unless (ty1 == ty2) $
    tell [UCEq ty1 ty2]

expectTypeEq :: (Eq a, EqRec (ty ki), Monad m) => Type ki ty a -> Type ki ty a -> UnifyT ki ty a m ()
expectTypeEq ty1 ty2 =
  unless (ty1 == ty2) $
    tell [UCEq ty1 ty2]

expectTypeAllEq :: (Eq a, EqRec (ty ki), Monad m) => NonEmpty (Type ki ty a) -> UnifyT ki ty a m (Type ki ty a)
expectTypeAllEq n@(ty :| tys) = do
  unless (all (== ty) tys ) $
    let
      xss = tails . N.toList $ n
      f [] = []
      f (x : xs) = fmap (UCEq x) xs
      ws = xss >>= f
    in
      tell ws
  return ty

type InferTypeContext e w s r m (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: (* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Ord a, OrdRec (ty ki), Bound (ty ki), Bitransversable (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsUnknownTypeError e, UnificationContext e m (Type ki ty) a)

prepareInferType :: InferTypeContext e w s r m ki ty pt tm a
             => (Type ki ty a -> UnifyT ki ty a m (Kind ki))
             -> (Type ki ty a -> Type ki ty a)
             -> InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
             -> InferTypeOutput e w s r m ki ty pt tm a
prepareInferType inferKindFn normalizeFn ii =
  let
    u = mkUnify _TyVar normalizeFn . iiUnifyRules $ ii
    pc = mkPCheck . iiPCheckRules $ ii
    i = mkInferType inferKindFn normalizeFn pc . iiInferTypeRules $ ii
    i' = mkInferType' i u
    c = mkCheck' i u
  in
    InferTypeOutput u i' c
