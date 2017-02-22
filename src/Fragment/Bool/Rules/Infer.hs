{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Bool.Rules.Infer (
    BoolInferContext
  , boolInferRules
  ) where

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Infer
import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common
import Util

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

inferBool :: (Monad m, AsTyBool ty, AsTmBool ty pt tm)
         => Term ty pt tm a
         -> Maybe (m (Type ty a))
inferBool tm = do
  _ <- preview _TmBool tm
  return . return . review _TyBool $ ()

inferAnd :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyBool ty, AsTmBool ty pt tm)
         => (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferAnd inferFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  return $ do
    let ty = review _TyBool ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

inferOr :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyBool ty, AsTmBool ty pt tm)
         => (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferOr inferFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  return $ do
    let ty = review _TyBool ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

checkBool :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsPtBool pt, AsTyBool ty) => Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkBool p ty = do
  _ <- preview _PtBool p
  return $ do
    let tyB = review _TyBool ()
    expect (ExpectedType tyB) (ActualType ty)
    return []

type BoolInferContext e s r m ty pt tm a = (InferContext e s r m ty pt tm a, AsTyBool ty, AsPtBool pt, AsTmBool ty pt tm)

boolInferRules :: BoolInferContext e s r m ty pt tm a
              => InferInput e s r m ty pt tm a
boolInferRules =
  InferInput
    [ InferBase inferBool
    , InferRecurse inferAnd
    , InferRecurse inferOr
    ]
    [ PCheckBase checkBool ]
