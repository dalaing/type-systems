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

import Data.Functor.Classes (Ord1)

import Bound (Bound)
import Control.Lens (review, preview)
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

type UnifyT ki a = WriterT [UConstraint (UnifyKind ki) a]

mkInferKind' :: (Functor ki, UnificationContext e m (UnifyKind ki) a, MonadError e m)
        => (Type ki ty a -> UnifyT ki a m (UnifyKind ki a))
        -> ([UConstraint (UnifyKind ki) a] -> m (M.Map a (UnifyKind ki a)))
        -> Type ki ty a
        -> m (UnifyKind ki a)
mkInferKind' go unifyFn x = do
  (ty, cs) <- runWriterT $ go x
  s <- unifyFn cs
  return $ mapSubst _UnifyKindVar s ty

data IKOffline

mkCheck' :: MkInferKindConstraint e w s r m ki ty a IKOffline
        => Proxy (MonadProxy e w s r m)
        -> (Type ki ty a -> UnifyT ki a m (UnifyKind ki a))
        -> ([UConstraint (UnifyKind ki) a] -> m (M.Map a (UnifyKind ki a)))
        -> Type ki ty a
        -> InferKind ki a IKOffline
        -> m ()
mkCheck' m inferFn unifyFn x y = do
  cs <- execWriterT $ (mkCheckKind m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) (Proxy :: Proxy IKOffline) inferFn) x y
  _ <- unifyFn cs
  return ()

instance MkInferKind IKOffline where
  type MkInferKindConstraint e w s r m ki ty a IKOffline =
    ( MonadError e m
    , AsUnknownKindError e
    , AsOccursError e (UnifyKind ki) a
    , AsUnificationMismatch e (UnifyKind ki) a
    , AsUnificationExpectedEq e (UnifyKind ki) a
    , AsUnboundTypeVariable e a
    , Ord a
    , Ord1 ki
    , OrdRec (ty ki)
    , Traversable ki
    , Bound (ty ki)
    , Bitransversable (ty ki)
    )
  type InferKindMonad ki a m IKOffline =
    UnifyT ki a m
  type InferKind ki a IKOffline =
    UnifyKind ki a
  type MkInferKindErrorList ki ty a IKOffline =
    '[ ErrOccursError (UnifyKind ki) a
     , ErrUnificationMismatch (UnifyKind ki) a
     , ErrUnificationExpectedEq (UnifyKind ki) a
     , ErrUnificationExpectedEq (Type ki ty) a
     , ErrUnboundTypeVariable a
     ]
  type MkInferKindWarningList ki ty a IKOffline =
    '[]

  mkKind _ _ _ _ _ =
    review _UKind

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
      u = mkUnify _UnifyKindVar id . kriUnifyRules $ kri
      i = mkInferKind . kriInferRules $ kri
      convertKind k =
        case preview _UKind k of
          Just ki -> return ki
          Nothing -> throwing _UnboundTypeVariable . head . toList $ k
      i' = (>>= convertKind) . mkInferKind' i u
      c ty ki = mkCheck' pm i u ty (mkKind pm pki pty pa pi ki)
    in
      InferKindOutput i' c
