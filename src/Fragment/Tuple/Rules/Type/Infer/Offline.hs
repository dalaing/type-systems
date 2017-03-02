{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Tuple.Rules.Type.Infer.Offline (
    TupleInferContext
  , tupleInferRules
  ) where

import Control.Monad (zipWithM, replicateM)

import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)
import Data.Equivalence.Monad (EquivT, classDesc)

import Rules.Infer.Unification
import Rules.Type.Infer.Offline
import Ast.Type
import Ast.Type.Var
import Ast.Pattern
import Ast.Term

import Fragment.Tuple.Ast.Type
import Fragment.Tuple.Ast.Error
import Fragment.Tuple.Ast.Pattern
import Fragment.Tuple.Ast.Term

equivTuple :: AsTyTuple ki ty => (Type ki ty a -> Type ki ty a -> Bool) -> Type ki ty a -> Type ki ty a -> Maybe Bool
equivTuple equivFn ty1 ty2 = do
  t1 <- preview _TyTuple ty1
  t2 <- preview _TyTuple ty2
  return . and $ zipWith equivFn t1 t2

unifyTuple :: (UnificationContext e m ki ty a, AsTyTuple ki ty)
          => ([Type ki ty a] -> [Type ki ty a] -> EquivT s (Type ki ty a) (Type ki ty a) m ())
          -> UConstraint ki ty a
          -> Maybe (EquivT s (Type ki ty a) (Type ki ty a) m ())
unifyTuple unifyMany u = do
  (ty1, ty2) <- preview _UCEq u
  tys1 <- preview _TyTuple ty1
  tys2 <- preview _TyTuple ty2
  return $ do
    cs1 <- traverse classDesc tys1
    cs2 <- traverse classDesc tys2
    unifyMany cs1 cs2

inferTmTuple :: (UnificationContext e m ki ty a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyTuple ki ty, AsTmTuple ki ty pt tm)
             => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
             -> Term ki ty pt tm a
             -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferTmTuple inferFn tm = do
  tms <- preview _TmTuple tm
  return $ do
    tys <- traverse inferFn tms
    tyV <- fmap (review _TyVar) freshTyVar
    expectTypeEq (review _TyTuple tys) tyV
    return tyV

inferTmTupleIx :: (UnificationContext e m ki ty a, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsExpectedTyTuple e ki ty a, AsTupleOutOfBounds e, AsTyTuple ki ty, AsTmTuple ki ty pt tm)
               => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
               -> Term ki ty pt tm a
               -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferTmTupleIx inferFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectTyTuple tyT
    tyVs <- replicateM (length tys) (fmap (review _TyVar) freshTyVar)
    expectTypeEq tyT (review _TyTuple tyVs)
    lookupTuple tyVs i
    -- lookupTuple tys i

checkTuple :: (MonadError e m, AsExpectedTyTuple e ki ty a, AsTyTuple ki ty, AsPtTuple pt) => (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkTuple checkFn p ty = do
  pts <- preview _PtTuple p
  return $ do
    tys <- expectTyTuple ty
    ms <- zipWithM checkFn pts tys
    return $ mconcat ms

type TupleInferContext e w s r m ki ty pt tm a = (InferContext e w s r m ki ty pt tm a, UnificationContext e m ki ty a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyTuple ki ty, AsExpectedTyTuple e ki ty a, AsTupleOutOfBounds e, AsPtTuple pt, AsTmTuple ki ty pt tm)

tupleInferRules :: TupleInferContext e w s r m ki ty pt tm a
                => InferInput e w s r m ki ty pt tm a
tupleInferRules =
  InferInput
    [ EquivRecurse equivTuple ]
    [ UnificationMany unifyTuple ]
    [ InferRecurse inferTmTuple
    , InferRecurse inferTmTupleIx
    ]
    [ PCheckRecurse checkTuple ]
