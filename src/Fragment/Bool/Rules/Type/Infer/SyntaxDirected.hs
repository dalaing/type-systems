{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Bool.Rules.Type.Infer.SyntaxDirected (
    BoolInferTypeContext
  , boolInferTypeRules
  ) where

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Type.Infer.SyntaxDirected
import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common
import Data.Functor.Rec

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

inferBool :: (Monad m, AsTyBool ki ty, AsTmBool ki ty pt tm)
         => Term ki ty pt tm a
         -> Maybe (m (Type ki ty a))
inferBool tm = do
  _ <- preview _TmBool tm
  return . return . review _TyBool $ ()

inferAnd :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsTyBool ki ty, AsTmBool ki ty pt tm)
         => (Term ki ty pt tm a -> m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (m (Type ki ty a))
inferAnd inferFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  return $ do
    let ty = review _TyBool ()
    mkCheckType inferFn tm1 ty
    mkCheckType inferFn tm2 ty
    return ty

inferOr :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsTyBool ki ty, AsTmBool ki ty pt tm)
         => (Term ki ty pt tm a -> m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (m (Type ki ty a))
inferOr inferFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  return $ do
    let ty = review _TyBool ()
    mkCheckType inferFn tm1 ty
    mkCheckType inferFn tm2 ty
    return ty

checkBool :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsPtBool pt, AsTyBool ki ty)
          => Pattern pt a
          -> Type ki ty a
          -> Maybe (m [Type ki ty a])
checkBool p ty = do
  _ <- preview _PtBool p
  return $ do
    let tyB = review _TyBool ()
    expectType (ExpectedType tyB) (ActualType ty)
    return []

type BoolInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, AsTyBool ki ty, AsPtBool pt, AsTmBool ki ty pt tm)

boolInferTypeRules :: BoolInferTypeContext e w s r m ki ty pt tm a
              => InferTypeInput e w s r m ki ty pt tm a
boolInferTypeRules =
  InferTypeInput
    [ InferTypeBase inferBool
    , InferTypeRecurse inferAnd
    , InferTypeRecurse inferOr
    ]
    [ PCheckBase checkBool ]
