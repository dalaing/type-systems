{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Int.Rules.Infer (
    IntInferContext
  , intInferRules
  ) where

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Infer
import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common
import Util

import Fragment.Int.Ast.Type
import Fragment.Int.Ast.Pattern
import Fragment.Int.Ast.Term

inferInt :: (Monad m, AsTyInt ty, AsTmInt ty pt tm)
         => Term ty pt tm a
         -> Maybe (m (Type ty a))
inferInt tm = do
  _ <- preview _TmInt tm
  return . return . review _TyInt $ ()

inferAdd :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyInt ty, AsTmInt ty pt tm)
         => (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferAdd inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    let ty = review _TyInt ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

inferMul :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyInt ty, AsTmInt ty pt tm)
         => (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferMul inferFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  return $ do
    let ty = review _TyInt ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

checkInt :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsPtInt pt, AsTyInt ty) => Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkInt p ty = do
  _ <- preview _PtInt p
  return $ do
    let tyI = review _TyInt ()
    expect (ExpectedType tyI) (ActualType ty)
    return []

type IntInferContext e s r m ty pt tm a = (InferContext e s r m ty pt tm a, AsTyInt ty, AsPtInt pt, AsTmInt ty pt tm)

intInferRules :: IntInferContext e s r m ty pt tm a
              => InferInput e s r m ty pt tm a
intInferRules =
  InferInput
    [ InferBase inferInt
    , InferRecurse inferAdd
    , InferRecurse inferMul
    ]
    [ PCheckBase checkInt ]
