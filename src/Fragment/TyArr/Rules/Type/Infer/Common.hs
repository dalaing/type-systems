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
{-# LANGUAGE TypeFamilies #-}
module Fragment.TyArr.Rules.Type.Infer.Common (
    TyArrInferTypeHelper(..)
  , TyArrInferTypeConstraint
  , tyArrInferTypeInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Ast.Type
import Data.Functor.Rec
import Rules.Unification

import Fragment.TyArr.Ast.Type

import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ISyntax)

import Rules.Type.Infer.Offline (IOffline)
import Control.Lens (preview)
import Control.Monad.Except (MonadError)
import Data.Equivalence.Monad (classDesc)

class MkInferType i => TyArrInferTypeHelper i where
  type TyArrInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a i :: Constraint

  unifyTyArrRules :: TyArrInferTypeHelperConstraint e w s r m ki ty a i
                 => Proxy (MonadProxy e w s r m)
                 -> Proxy i
                 -> [UnificationRule m (Type ki ty) a]

instance TyArrInferTypeHelper ISyntax where
  type TyArrInferTypeHelperConstraint e w s r m ki ty a ISyntax =
    (() :: Constraint)

  unifyTyArrRules _ _  =
    []

instance TyArrInferTypeHelper IOffline where
  type TyArrInferTypeHelperConstraint e w s r m ki ty a IOffline =
    ( AsTyArr ki ty
    , Ord a
    , OrdRec (ty ki)
    , MonadError e m
    , AsOccursError e (Type ki ty) a
    , AsUnificationMismatch e (Type ki ty) a
    , AsUnificationExpectedEq e (Type ki ty) a
    )

  unifyTyArrRules _ _  =
    let
      unifyTyArr unifyMany (UCEq ty1 ty2) = do
        (p1a, p1b) <- preview _TyArr ty1
        (p2a, p2b) <- preview _TyArr ty2
        return $ do
          c1a <- classDesc p1a
          c1b <- classDesc p1b
          c2a <- classDesc p2a
          c2b <- classDesc p2b
          unifyMany [c1a, c1b] [c2a, c2b]
    in
      [ UnificationMany unifyTyArr ]

type TyArrInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , TyArrInferTypeHelper i
  , TyArrInferTypeHelperConstraint e w s r m ki ty a i
  )

tyArrInferTypeInput :: TyArrInferTypeConstraint e w s r m ki ty pt tm a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
tyArrInferTypeInput m i =
  InferTypeInput (unifyTyArrRules m i) [] []
