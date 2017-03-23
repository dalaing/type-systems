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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fragment.Bool.Rules.Type.Infer.Common (
    BoolInferTypeHelper(..)
  , BoolInferTypeConstraint
  , boolInferTypeInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Control.Lens (review, preview)

import Ast.Type
import Ast.Pattern
import Ast.Error.Common
import Ast.Term

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ISyntax)

import Rules.Type.Infer.Offline (IOffline)
import Ast.Type.Var
import Control.Monad.State (MonadState)

class MkInferType i => BoolInferTypeHelper i where
  type BoolInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a i :: Constraint

  createBool :: BoolInferTypeHelperConstraint e w s r m ki ty a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> InferTypeMonad ki ty a m i (Type ki ty a)

instance BoolInferTypeHelper ISyntax where
  type BoolInferTypeHelperConstraint e w s r m ki ty a ISyntax =
    ( AsTyBool ki ty
    , Monad m
    )

  createBool _ _ =
    return . review _TyBool $ ()

instance BoolInferTypeHelper IOffline where
  type BoolInferTypeHelperConstraint e w s r m ki ty a IOffline =
    ( MonadState s m
    , HasTyVarSupply s
    , ToTyVar a
    )

  createBool _ _ =
    fmap (review _TyVar) freshTyVar

type BoolInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BoolInferConstraint e w s r m ki ty pt tm a i
  , BoolCheckConstraint e w s r m ki ty pt tm a i
  )

type BoolInferConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , BoolInferTypeHelper i
  , BoolInferTypeHelperConstraint e w s r m ki ty a i
  , AsTmBool ki ty pt tm
  , AsTyBool ki ty
  )

type BoolCheckConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , AsPtBool pt
  , AsTyBool ki ty
  )

boolInferTypeInput :: BoolInferTypeConstraint e w s r m ki ty pt tm a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
boolInferTypeInput m i =
  InferTypeInput
    []
    [ InferTypeBase $ inferTmBool m i
    , InferTypeRecurse $ inferTmAnd m i
    , InferTypeRecurse $ inferTmOr m i
    ]
    [ PCheckBase $ checkBool m i]

inferTmBool :: BoolInferConstraint e w s r m ki ty pt tm a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy i
            -> Term ki ty pt tm a
            -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmBool _ _ tm = do
  _ <- preview _TmBool tm
  return . return . review _TyBool $ ()

inferTmAnd :: BoolInferConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmAnd m i inferFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  return $ do
    let ty = review _TyBool ()
    ty1 <- inferFn tm1
    expectType m i (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType m i (ExpectedType ty) (ActualType ty2)
    tyV <- createBool m i
    expectType m i (ExpectedType ty) (ActualType tyV)
    return tyV

inferTmOr :: BoolInferConstraint e w s r m ki ty pt tm a i
          => Proxy (MonadProxy e w s r m)
          -> Proxy i
          -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
          -> Term ki ty pt tm a
          -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmOr m i inferFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  return $ do
    let ty = review _TyBool ()
    ty1 <- inferFn tm1
    expectType m i (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType m i (ExpectedType ty) (ActualType ty2)
    tyV <- createBool m i
    expectType m i (ExpectedType ty) (ActualType tyV)
    return tyV

checkBool :: BoolCheckConstraint e w s r m ki ty pt tm a i
          => Proxy (MonadProxy e w s r m)
          -> Proxy i
          -> Pattern pt a
          -> Type ki ty a
          -> Maybe (InferTypeMonad ki ty a m i [Type ki ty a])
checkBool m i p ty = do
  _ <- preview _PtBool p
  return $ do
    let tyI = review _TyBool ()
    expectType m i (ExpectedType tyI) (ActualType ty)
    return []
