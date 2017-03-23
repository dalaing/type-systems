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
module Fragment.Int.Rules.Type.Infer.Common (
    IntInferTypeHelper(..)
  , IntInferTypeConstraint
  , intInferTypeInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Control.Lens (review, preview)

import Ast.Type
import Ast.Pattern
import Ast.Error.Common
import Ast.Term

import Fragment.Int.Ast.Type
import Fragment.Int.Ast.Pattern
import Fragment.Int.Ast.Term

import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ISyntax)

import Rules.Type.Infer.Offline (IOffline)
import Ast.Type.Var
import Control.Monad.State (MonadState)
 

class MkInferType i => IntInferTypeHelper i where
  type IntInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a i :: Constraint

  createInt :: IntInferTypeHelperConstraint e w s r m ki ty a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy i
            -> InferTypeMonad ki ty a m i (Type ki ty a)

instance IntInferTypeHelper ISyntax where
  type IntInferTypeHelperConstraint e w s r m ki ty a ISyntax =
    ( AsTyInt ki ty
    , Monad m
    )

  createInt _ _ =
    return . review _TyInt $ ()

instance IntInferTypeHelper IOffline where
  type IntInferTypeHelperConstraint e w s r m ki ty a IOffline =
    ( MonadState s m
    , HasTyVarSupply s
    , ToTyVar a
    )

  createInt _ _ =
    fmap (review _TyVar) freshTyVar

type IntInferTypeConstraint e w s r m ki ty pt tm a i =
  ( IntInferConstraint e w s r m ki ty pt tm a i
  , IntCheckConstraint e w s r m ki ty pt tm a i
  )

type IntInferConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , IntInferTypeHelper i
  , IntInferTypeHelperConstraint e w s r m ki ty a i
  , AsTmInt ki ty pt tm
  , AsTyInt ki ty
  )

type IntCheckConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , AsPtInt pt
  , AsTyInt ki ty
  )

intInferTypeInput :: IntInferTypeConstraint e w s r m ki ty pt tm a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
intInferTypeInput m i =
  InferTypeInput
    []
    [ InferTypeBase $ inferTmInt m i
    , InferTypeRecurse $ inferTmAdd m i
    , InferTypeRecurse $ inferTmSub m i
    , InferTypeRecurse $ inferTmMul m i
    ]
    [ PCheckBase $ checkInt m i]

inferTmInt :: IntInferConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmInt _ _ tm = do
  _ <- preview _TmInt tm
  return . return . review _TyInt $ ()

inferTmAdd :: IntInferConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmAdd m i inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    let ty = review _TyInt ()
    ty1 <- inferFn tm1
    expectType m i (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType m i (ExpectedType ty) (ActualType ty2)
    tyV <- createInt m i
    expectType m i (ExpectedType ty) (ActualType tyV)
    return tyV

inferTmSub :: IntInferConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmSub m i inferFn tm = do
  (tm1, tm2) <- preview _TmSub tm
  return $ do
    let ty = review _TyInt ()
    ty1 <- inferFn tm1
    expectType m i (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType m i (ExpectedType ty) (ActualType ty2)
    tyV <- createInt m i
    expectType m i (ExpectedType ty) (ActualType tyV)
    return tyV

inferTmMul :: IntInferConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmMul m i inferFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  return $ do
    let ty = review _TyInt ()
    ty1 <- inferFn tm1
    expectType m i (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType m i (ExpectedType ty) (ActualType ty2)
    tyV <- createInt m i
    expectType m i (ExpectedType ty) (ActualType tyV)
    return tyV

checkInt :: IntCheckConstraint e w s r m ki ty pt tm a i
         => Proxy (MonadProxy e w s r m)
         -> Proxy i
         -> Pattern pt a
         -> Type ki ty a
         -> Maybe (InferTypeMonad ki ty a m i [Type ki ty a])
checkInt m i p ty = do
  _ <- preview _PtInt p
  return $ do
    let tyI = review _TyInt ()
    expectType m i (ExpectedType tyI) (ActualType ty)
    return []
