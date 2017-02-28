{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Int.Rules.Infer.Unification.Offline (
    IntInferContext
  , intInferRules
  ) where

import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Infer.Unification.Offline
import Ast.Type
import Ast.Type.Var
import Ast.Pattern
import Ast.Term
import Ast.Error.Common
import Data.Functor.Rec

import Fragment.Int.Ast.Type
import Fragment.Int.Ast.Pattern
import Fragment.Int.Ast.Term

equivInt :: AsTyInt ty => Type ty a -> Type ty a -> Maybe Bool
equivInt ty1 ty2 = do
  _ <- preview _TyInt ty1
  _ <- preview _TyInt ty2
  return True

inferInt :: (Monad m, AsTyInt ty, AsTmInt ty pt tm)
         => Term ty pt tm a
         -> Maybe (m (Type ty a))
inferInt tm = do
  _ <- preview _TmInt tm
  return . return . review _TyInt $ ()

inferAdd :: (Eq a, EqRec ty, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpected e ty a, AsTyInt ty, AsTmInt ty pt tm)
         => (Term ty pt tm a -> UnifyT ty a m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (UnifyT ty a m (Type ty a))
inferAdd inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    tyV <- fmap (review _TyVar) freshTyVar
    let ty = review _TyInt ()
    expectEq ty1 ty
    expectEq ty2 ty
    expectEq tyV ty
    return tyV

inferMul :: (Eq a, EqRec ty, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpected e ty a, AsTyInt ty, AsTmInt ty pt tm)
         => (Term ty pt tm a -> UnifyT ty a m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (UnifyT ty a m (Type ty a))
inferMul inferFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    tyV <- fmap (review _TyVar) freshTyVar
    let ty = review _TyInt ()
    expectEq ty1 ty
    expectEq ty2 ty
    expectEq tyV ty
    return tyV

checkInt :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsPtInt pt, AsTyInt ty)
         => Pattern pt a
         -> Type ty a
         -> Maybe (UnifyT ty a m [Type ty a])
checkInt p ty = do
  _ <- preview _PtInt p
  return $ do
    let tyI = review _TyInt ()
    expect (ExpectedType tyI) (ActualType ty)
    return []

type IntInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyInt ty, AsPtInt pt, AsTmInt ty pt tm)

intInferRules :: IntInferContext e w s r m ty pt tm a
              => InferInput e w s r m ty pt tm a
intInferRules =
  InferInput
    [ EquivBase equivInt ]
    []
    [ InferBase inferInt
    , InferRecurse inferAdd
    , InferRecurse inferMul
    ]
    [ PCheckBase checkInt ]
