{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Rules.Kind.Infer.Offline (
    UnifyT
  , IKOffline
  ) where

import Control.Monad (unless)
import Data.Foldable (toList)
import Data.List (tails)
import Data.Proxy (Proxy(..))


import Bound (Bound)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT, runWriterT)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N

import qualified Data.Map as M

import Data.Bitransversable
import Data.Functor.Rec

import Ast.Kind
import Ast.Type
import Ast.Error.Common
import Context.Type.Error

import Rules.Unification

import Rules.Kind.Infer.Common

type UnifyT ki a = WriterT [UConstraint (Kind ki) a]

mkInferKind' :: (UnificationContext e m (Kind ki) a, MonadError e m)
        => (Type ki ty a -> UnifyT ki a m (Kind ki a))
        -> ([UConstraint (Kind ki) a] -> m (M.Map a (Kind ki a)))
        -> Type ki ty a
        -> m (Kind ki a)
mkInferKind' go unifyFn x = do
  (ty, cs) <- runWriterT $ go x
  s <- unifyFn cs
  return $ mapSubst _KiVar s ty

data IKOffline

mkCheck' :: MkInferKindConstraint e w s r m ki ty a IKOffline
        => Proxy (MonadProxy e w s r m)
        -> (Type ki ty a -> UnifyT ki a m (Kind ki a))
        -> ([UConstraint (Kind ki) a] -> m (M.Map a (Kind ki a)))
        -> Type ki ty a
        -> Kind ki a
        -> m ()
mkCheck' m inferFn unifyFn x y = do
  cs <- execWriterT $ (mkCheckKind m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) (Proxy :: Proxy IKOffline) inferFn) x y
  _ <- unifyFn cs
  return ()

instance MkInferKind IKOffline where
  type MkInferKindConstraint e w s r m ki ty a IKOffline =
    ( MonadError e m
    , AsUnknownKindError e
    , AsOccursError e (Kind ki) a
    , AsUnificationMismatch e (Kind ki) a
    , AsUnificationExpectedEq e (Kind ki) a
    , AsUnboundTypeVariable e a
    , Ord a
    , OrdRec ki
    , OrdRec (ty ki)
    , Bound ki
    , Bitransversable ki
    , Bound (ty ki)
    , Bitransversable (ty ki)
    )
  type InferKindMonad m ki a IKOffline =
    UnifyT ki a m
  type MkInferKindErrorList ki ty a IKOffline =
    '[ ErrOccursError (Kind ki) a
     , ErrUnificationMismatch (Kind ki) a
     , ErrUnificationExpectedEq (Kind ki) a
     , ErrUnificationExpectedEq (Type ki ty) a
     , ErrUnboundTypeVariable a
     ]
  type MkInferKindWarningList ki ty a IKOffline =
    '[]

  mkCheckKind m ki ty a i =
    mkCheckKind' i (expectKind m ki ty a i)

  expectKind _ _ _ _ _ (ExpectedKind ki1) (ActualKind ki2) =
    unless (ki1 == ki2) $
      tell [UCEq ki1 ki2]

  expectKindEq _ _ _ _ _ ki1 ki2 =
    unless (ki1 == ki2) $
      tell [UCEq ki1 ki2]

  expectKindAllEq _ _ _ _ _ n@(ki :| kis) = do
    unless (all (== ki) kis) $
      let
        xss = tails . N.toList $ n
        f [] = []
        f (x : xs) = fmap (UCEq x) xs
        ws = xss >>= f
      in
        tell ws
    return ki

  prepareInferKind pm pki pty pa pi kri =
    let
      u = mkUnify _KiVar id . kriUnifyRules $ kri
      i = mkInferKind . kriInferRules $ kri
      convertKind k =
        case toList k of
          [] -> return k
          (x : _) -> throwing _UnboundTypeVariable x
      i' = (>>= convertKind) . mkInferKind' i u
      c = mkCheck' pm i u
    in
      InferKindOutput i' c
