{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Rules.Type.Infer.Offline (
    UnifyT
  , ITOffline
  ) where

import Control.Monad (unless)
import Data.Functor.Identity (Identity(..))
import Data.List (tails)
import Data.Proxy (Proxy(..))

import Bound (Bound)
import Control.Lens (review)
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
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

type UnifyT ki ty a = WriterT [UConstraint (TyAst ki ty) (TyAstVar a)]

mkInferType' :: (Ord a, UnificationContext e m (TyAst ki ty) (TyAstVar a), MonadError e m, AsUnknownTypeError e)
        => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
        -> ([UConstraint (TyAst ki ty) (TyAstVar a)] -> m (M.Map (TyAstVar a) (TyAst ki ty (TyAstVar a))))
        -> Term ki ty pt tm a
        -> m (Type ki ty a)
mkInferType' go unifyFn x = do
  (ty, cs) <- runWriterT $ go x
  let ty' = review _Unwrapped ty
  s <- unifyFn cs
  let ty'' = mapSubst _TyAstVar s ty'
  return $ review _Wrapped ty''

data ITOffline

mkCheck' :: MkInferTypeConstraint e w s r m ki ty a ITOffline
        => Proxy (MonadProxy e w s r m)
        -> (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
        -> ([UConstraint (TyAst ki ty) (TyAstVar a)] -> m (M.Map (TyAstVar a) (TyAst ki ty (TyAstVar a))))
        -> Term ki ty pt tm a
        -> Type ki ty a
        -> m ()
mkCheck' m inferFn unifyFn x y = do
  cs <- execWriterT $ (mkCheckType m (Proxy :: Proxy ITOffline) inferFn) x y
  _ <- unifyFn cs
  return ()

fixupNormalize :: (Type ki ty a -> Type ki ty a)
               -> TyAst ki ty (TyAstVar a)
               -> TyAst ki ty (TyAstVar a)
fixupNormalize fn =
  review _Unwrapped .
  fn .
  review _Wrapped

fixupUnify :: Monad m
           => ([UConstraint (TyAst ki ty) (TyAstVar a)] -> m (M.Map (TyAstVar a) (TyAst ki ty (TyAstVar a))))
           -> [UConstraint (Type ki ty) a]
           -> m (M.Map a (Type ki ty a))
fixupUnify u =
  let
    fixConstraint (UCEq a1 a2) = UCEq (review _Unwrapped a1) (review _Unwrapped a2)
    fixMap = undefined
  in
    fmap fixMap .
    u .
    fmap fixConstraint

instance MkInferType ITOffline where
  type MkInferTypeConstraint e w s r m ki ty a ITOffline =
    ( Ord a
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
  type InferTypeMonad m ki ty a ITOffline =
    UnifyT ki ty a m
  type MkInferTypeErrorList ki ty pt tm a ITOffline =
    '[ ErrOccursError (TyAst ki ty) (TyAstVar a)
     , ErrUnificationMismatch (TyAst ki ty) (TyAstVar a)
     , ErrUnificationExpectedEq (TyAst ki ty) (TyAstVar a)
     ]
  type MkInferTypeWarningList ki ty pt tm a ITOffline =
    '[]

  mkCheckType m i =
    mkCheckType' (expectType m i)

  expectType _ _ (ExpectedType ty1) (ActualType ty2) =
    unless (ty1 == ty2) $
      tell [UCEq (review _Unwrapped ty1) (review _Unwrapped ty2)]

  expectTypeEq _ _ ty1 ty2 =
    unless (ty1 == ty2) $
      tell [UCEq (review _Unwrapped ty1) (review _Unwrapped ty2)]

  expectTypeAllEq _ _ n@(ty :| tys) = do
    unless (all (== ty) tys ) $
      let
        xss = tails . N.toList $ n
        f [] = []
        f (x : xs) = fmap (UCEq (review _Unwrapped x)) (fmap (review _Unwrapped) xs)
        ws = xss >>= f
      in
        tell ws
    return ty

  prepareInferType pm pi inferKindFn normalizeFn ii =
    let
      n = fixupNormalize normalizeFn
      u = mkUnify _TyAstVar n . iiUnifyRules $ ii
      u' = fixupUnify u
      pc = mkPCheck . iiPCheckRules $ ii
      i = mkInferType inferKindFn normalizeFn pc . iiInferTypeRules $ ii
      i' = mkInferType' i u
      c = mkCheck' pm i u
    in
      InferTypeOutput u' i' c
