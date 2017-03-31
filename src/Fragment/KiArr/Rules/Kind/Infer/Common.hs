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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.KiArr.Rules.Kind.Infer.Common (
    KiArrInferKindConstraint
  , kiArrInferKindInput
  ) where

import Data.Proxy (Proxy)
import GHC.Exts (Constraint)

import Control.Lens (preview)
import Control.Monad.Except (MonadError)
import Data.Equivalence.Monad (classDesc)

import Ast.Kind
import Ast.Error.Common.Kind
import Data.Functor.Rec

import Fragment.KiArr.Ast.Kind

import Rules.Kind.Infer.Common

import Rules.Kind.Infer.SyntaxDirected (IKSyntax)
import Rules.Kind.Infer.Offline (IKOffline)
import Rules.Unification

class MkInferKind i => KiArrInferKindHelper i where
  type KiArrInferKindHelperConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) a i :: Constraint

  unifyKiArrRules :: KiArrInferKindHelperConstraint e w s r m ki a i
                  => Proxy (MonadProxy e w s r m)
                  -> Proxy i
                  -> [UnificationRule m (Kind ki) a]

instance KiArrInferKindHelper IKSyntax where
  type KiArrInferKindHelperConstraint e w s r m ki a IKSyntax =
    (() :: Constraint)

  unifyKiArrRules _ _  =
    []

instance KiArrInferKindHelper IKOffline where
  type KiArrInferKindHelperConstraint e w s r m ki a IKOffline =
    ( AsKiArr ki
    , Ord a
    , OrdRec ki
    , MonadError e m
    , AsUnknownKindError e
    , AsOccursError e (Kind ki) a
    , AsUnificationMismatch e (Kind ki) a
    , AsUnificationExpectedEq e (Kind ki) a
    )

  unifyKiArrRules _ _  =
    let
      unifyKiArr unifyMany (UCEq ki1 ki2) = do
        (p1a, p1b) <- preview _KiArr ki1
        (p2a, p2b) <- preview _KiArr ki2
        return $ do
          c1a <- classDesc p1a
          c1b <- classDesc p1b
          c2a <- classDesc p2a
          c2b <- classDesc p2b
          unifyMany [c1a, c1b] [c2a, c2b]
    in
      [ UnificationMany unifyKiArr ]

type KiArrInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , KiArrInferKindHelper i
  , KiArrInferKindHelperConstraint e w s r m ki a i
  )

kiArrInferKindInput :: KiArrInferKindConstraint e w s r m ki ty a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferKindInput e w s r m (InferKindMonad ki a m i) ki ty a
kiArrInferKindInput m i =
  InferKindInput
    (unifyKiArrRules m i)
    []
