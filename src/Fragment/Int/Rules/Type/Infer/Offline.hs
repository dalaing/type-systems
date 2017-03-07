{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Int.Rules.Type.Infer.Offline (
    IntInferTypeContext
  , intInferTypeRules
  ) where

import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Type.Infer.Offline
import Ast.Type
import Ast.Type.Var
import Ast.Pattern
import Ast.Term
import Ast.Error.Common
import Data.Functor.Rec

import Fragment.Int.Ast.Type
import Fragment.Int.Ast.Pattern
import Fragment.Int.Ast.Term

inferInt :: (Monad m, AsTyInt ki ty, AsTmInt ki ty pt tm)
         => Term ki ty pt tm a
         -> Maybe (m (Type ki ty a))
inferInt tm = do
  _ <- preview _TmInt tm
  return . return . review _TyInt $ ()

inferAdd :: (Eq a, EqRec (ty ki), MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpectedType e ki ty a, AsTyInt ki ty, AsTmInt ki ty pt tm)
         => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferAdd inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    tyV <- fmap (review _TyVar) freshTyVar
    let ty = review _TyInt ()
    expectTypeEq ty1 ty
    expectTypeEq ty2 ty
    expectTypeEq tyV ty
    return tyV

inferMul :: (Eq a, EqRec (ty ki), MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpectedType e ki ty a, AsTyInt ki ty, AsTmInt ki ty pt tm)
         => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferMul inferFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    tyV <- fmap (review _TyVar) freshTyVar
    let ty = review _TyInt ()
    expectTypeEq ty1 ty
    expectTypeEq ty2 ty
    expectTypeEq tyV ty
    return tyV

checkInt :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsPtInt pt, AsTyInt ki ty)
         => Pattern pt a
         -> Type ki ty a
         -> Maybe (UnifyT ki ty a m [Type ki ty a])
checkInt p ty = do
  _ <- preview _PtInt p
  return $ do
    let tyI = review _TyInt ()
    expectType (ExpectedType tyI) (ActualType ty)
    return []

type IntInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyInt ki ty, AsPtInt pt, AsTmInt ki ty pt tm)

intInferTypeRules :: IntInferTypeContext e w s r m ki ty pt tm a
              => InferTypeInput e w s r m ki ty pt tm a
intInferTypeRules =
  InferTypeInput
    []
    [ InferTypeBase inferInt
    , InferTypeRecurse inferAdd
    , InferTypeRecurse inferMul
    ]
    [ PCheckBase checkInt ]
