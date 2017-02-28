{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Bool.Rules.Infer.Unification.Offline (
    BoolInferContext
  , boolInferRules
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

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

equivBool :: AsTyBool ty => Type ty a -> Type ty a -> Maybe Bool
equivBool ty1 ty2 = do
  _ <- preview _TyBool ty1
  _ <- preview _TyBool ty2
  return True

inferBool :: (Monad m, AsTyBool ty, AsTmBool ty pt tm)
         => Term ty pt tm a
         -> Maybe (UnifyT ty a m (Type ty a))
inferBool tm = do
  _ <- preview _TmBool tm
  return . return . review _TyBool $ ()

inferAnd :: (Eq a, EqRec ty, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpected e ty a, AsTyBool ty, AsTmBool ty pt tm)
         => (Term ty pt tm a -> UnifyT ty a m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (UnifyT ty a m (Type ty a))
inferAnd inferFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    tyV <- fmap (review _TyVar) freshTyVar
    let ty = review _TyBool ()
    expectEq ty1 ty
    expectEq ty2 ty
    expectEq tyV ty
    return tyV

inferOr :: (Eq a, EqRec ty, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsUnexpected e ty a, AsTyBool ty, AsTmBool ty pt tm)
         => (Term ty pt tm a -> UnifyT ty a m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (UnifyT ty a m (Type ty a))
inferOr inferFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    tyV <- fmap (review _TyVar) freshTyVar
    let ty = review _TyBool ()
    expectEq ty1 ty
    expectEq ty2 ty
    expectEq tyV ty
    return tyV

checkBool :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsPtBool pt, AsTyBool ty)
          => Pattern pt a
          -> Type ty a
          -> Maybe (UnifyT ty a m [Type ty a])
checkBool p ty = do
  _ <- preview _PtBool p
  return $ do
    let tyB = review _TyBool ()
    expect (ExpectedType tyB) (ActualType ty)
    return []

type BoolInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, MonadState s m, HasTyVarSupply s , ToTyVar a, AsTyBool ty, AsPtBool pt, AsTmBool ty pt tm)

boolInferRules :: BoolInferContext e w s r m ty pt tm a
              => InferInput e w s r m ty pt tm a
boolInferRules =
  InferInput
    [ EquivBase equivBool ]
    []
    [ InferBase inferBool
    , InferRecurse inferAnd
    , InferRecurse inferOr
    ]
    [ PCheckBase checkBool ]
