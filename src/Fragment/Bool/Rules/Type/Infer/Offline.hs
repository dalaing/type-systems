{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Bool.Rules.Type.Infer.Offline (
    BoolInferTypeContext
  , boolInferTypeRules
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

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

inferBool :: (Monad m, AsTyBool ki ty, AsTmBool ki ty pt tm)
         => Term ki ty pt tm a
         -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferBool tm = do
  _ <- preview _TmBool tm
  return . return . review _TyBool $ ()

inferAnd :: (Eq a, EqRec (ty ki), MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpectedType e ki ty a, AsTyBool ki ty, AsTmBool ki ty pt tm)
         => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferAnd inferFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    tyV <- fmap (review _TyVar) freshTyVar
    let ty = review _TyBool ()
    expectTypeEq ty1 ty
    expectTypeEq ty2 ty
    expectTypeEq tyV ty
    return tyV

inferOr :: (Eq a, EqRec (ty ki), MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpectedType e ki ty a, AsTyBool ki ty, AsTmBool ki ty pt tm)
         => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferOr inferFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    tyV <- fmap (review _TyVar) freshTyVar
    let ty = review _TyBool ()
    expectTypeEq ty1 ty
    expectTypeEq ty2 ty
    expectTypeEq tyV ty
    return tyV

checkBool :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpectedType e ki ty a, AsPtBool pt, AsTyBool ki ty)
          => Pattern pt a
          -> Type ki ty a
          -> Maybe (UnifyT ki ty a m [Type ki ty a])
checkBool p ty = do
  _ <- preview _PtBool p
  return $ do
    let tyB = review _TyBool ()
    expectType (ExpectedType tyB) (ActualType ty)
    return []

type BoolInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyBool ki ty, AsPtBool pt, AsTmBool ki ty pt tm)

boolInferTypeRules :: BoolInferTypeContext e w s r m ki ty pt tm a
              => InferTypeInput e w s r m ki ty pt tm a
boolInferTypeRules =
  InferTypeInput
    []
    [ InferTypeBase inferBool
    , InferTypeRecurse inferAnd
    , InferTypeRecurse inferOr
    ]
    [ PCheckBase checkBool ]
