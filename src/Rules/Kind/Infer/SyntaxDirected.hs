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
module Rules.Kind.Infer.SyntaxDirected (
    IKSyntax
  ) where

import Control.Monad (unless)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Data.List.NonEmpty (NonEmpty(..))

import Ast.Error.Common
import Data.Functor.Rec

import Rules.Kind.Infer.Common

data IKSyntax

instance MkInferKind IKSyntax where
  type MkInferKindConstraint e w s r m ki ty a IKSyntax =
    ( Eq a
    , EqRec ki
    , MonadError e m
    , AsUnknownKindError e
    , AsUnexpectedKind e ki a
    , AsExpectedKindEq e ki a
    , AsExpectedKindAllEq e ki a
    )
  type InferKindMonad m ki a IKSyntax =
    m
  type MkInferKindErrorList ki ty a IKSyntax =
    '[]
  type MkInferKindWarningList ki ty a IKSyntax =
    '[]

  mkCheckKind m ki ty a i =
    mkCheckKind' i (expectKind m ki ty a i)

  expectKind _ _ _ _ _ e@(ExpectedKind ki1) a@(ActualKind ki2) =
    unless (ki1 == ki2) $
      throwing _UnexpectedKind (e, a)

  expectKindEq _ _ _ _ _ ki1 ki2 =
    unless (ki1 == ki2) $
      throwing _ExpectedKindEq (ki1, ki2)

  expectKindAllEq _ _ _ _ _ (ki :| kis) = do
    unless (all (== ki) kis) $
      throwing _ExpectedKindAllEq (ki :| kis)
    return ki

  prepareInferKind pm pki pty pa pi ki =
    let
      i = mkInferKind . kriInferRules $ ki
      c = mkCheckKind pm pki pty pa pi i
    in
      InferKindOutput i c
