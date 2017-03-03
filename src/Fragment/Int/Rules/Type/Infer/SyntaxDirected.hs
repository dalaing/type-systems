{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Int.Rules.Type.Infer.SyntaxDirected (
    IntInferContext
  , intInferRules
  ) where

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Type.Infer.SyntaxDirected
import Ast.Type
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

inferAdd :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsTyInt ki ty, AsTmInt ki ty pt tm)
         => (Term ki ty pt tm a -> m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (m (Type ki ty a))
inferAdd inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    let ty = review _TyInt ()
    mkCheckType inferFn tm1 ty
    mkCheckType inferFn tm2 ty
    return ty

inferMul :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsTyInt ki ty, AsTmInt ki ty pt tm)
         => (Term ki ty pt tm a -> m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (m (Type ki ty a))
inferMul inferFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  return $ do
    let ty = review _TyInt ()
    mkCheckType inferFn tm1 ty
    mkCheckType inferFn tm2 ty
    return ty

checkInt :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsPtInt pt, AsTyInt ki ty)
         => Pattern pt a
         -> Type ki ty a
         -> Maybe (m [Type ki ty a])
checkInt p ty = do
  _ <- preview _PtInt p
  return $ do
    let tyI = review _TyInt ()
    expectType (ExpectedType tyI) (ActualType ty)
    return []

type IntInferContext e w s r m ki ty pt tm a = (InferContext e w s r m ki ty pt tm a, AsTyInt ki ty, AsPtInt pt, AsTmInt ki ty pt tm)

intInferRules :: IntInferContext e w s r m ki ty pt tm a
              => InferInput e w s r m ki ty pt tm a
intInferRules =
  InferInput
    [ InferBase inferInt
    , InferRecurse inferAdd
    , InferRecurse inferMul
    ]
    [ PCheckBase checkInt ]
