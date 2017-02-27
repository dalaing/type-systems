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

inferAdd :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyInt ty, AsTmInt ty pt tm)
         => (Type ty a -> Type ty a -> Bool)
         -> (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferAdd tyEquiv inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    let ty = review _TyInt ()
    mkCheck tyEquiv inferFn tm1 ty
    mkCheck tyEquiv inferFn tm2 ty
    return ty

inferMul :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyInt ty, AsTmInt ty pt tm)
         => (Type ty a -> Type ty a -> Bool)
         -> (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferMul tyEquiv inferFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  return $ do
    let ty = review _TyInt ()
    mkCheck tyEquiv inferFn tm1 ty
    mkCheck tyEquiv inferFn tm2 ty
    return ty

checkInt :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsPtInt pt, AsTyInt ty) => (Type ty a -> Type ty a -> Bool) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkInt tyEquiv p ty = do
  _ <- preview _PtInt p
  return $ do
    let tyI = review _TyInt ()
    expect tyEquiv (ExpectedType tyI) (ActualType ty)
    return []

type IntInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, AsTyInt ty, AsPtInt pt, AsTmInt ty pt tm)

intInferRules :: IntInferContext e w s r m ty pt tm a
              => InferInput e w s r m ty pt tm a
intInferRules =
  InferInput
    [ EquivBase equivInt ]
    [ InferBase inferInt
    , InferTyEquivRecurse inferAdd
    , InferTyEquivRecurse inferMul
    ]
    [ PCheckTyEquiv checkInt ]
