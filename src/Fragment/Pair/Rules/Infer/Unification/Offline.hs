{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Pair.Rules.Infer.Unification.Offline (
    PairInferContext
  , pairInferRules
  ) where

import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)
import Data.Equivalence.Monad (EquivT, classDesc)

import Rules.Infer.Unification
import Rules.Infer.Unification.Offline
import Ast.Type
import Ast.Type.Var
import Ast.Error.Common
import Ast.Pattern
import Ast.Term


import Fragment.Pair.Ast.Type
import Fragment.Pair.Ast.Error
import Fragment.Pair.Ast.Pattern
import Fragment.Pair.Ast.Term

equivPair :: AsTyPair ki ty => (Type ki ty a -> Type ki ty a -> Bool) -> Type ki ty a -> Type ki ty a -> Maybe Bool
equivPair equivFn ty1 ty2 = do
  (p1a, p1b) <- preview _TyPair ty1
  (p2a, p2b) <- preview _TyPair ty2
  return $ equivFn p1a p2a && equivFn p1b p2b

unifyPair :: (UnificationContext e m ki ty a, AsTyPair ki ty)
          => ([Type ki ty a] -> [Type ki ty a] -> EquivT s (Type ki ty a) (Type ki ty a) m ())
          -> UConstraint ki ty a
          -> Maybe (EquivT s (Type ki ty a) (Type ki ty a) m ())
unifyPair unifyMany u = do
  (ty1, ty2) <- preview _UCEq u
  (p1a, p1b) <- preview _TyPair ty1
  (p2a, p2b) <- preview _TyPair ty2
  return $ do
    c1a <- classDesc p1a
    c1b <- classDesc p1b
    c2a <- classDesc p2a
    c2b <- classDesc p2b
    unifyMany [c1a, c1b] [c2a, c2b]

inferTmPair :: (UnificationContext e m ki ty a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyPair ki ty, AsTmPair ki ty pt tm)
            => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
            -> Term ki ty pt tm a
            -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferTmPair inferFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    tyV <- fmap (review _TyVar) freshTyVar
    expectEq (review _TyPair (ty1, ty2)) tyV
    return tyV

inferTmFst :: (UnificationContext e m ki ty a, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsExpectedEq e ki ty a, AsExpectedTyPair e ki ty a, AsTyPair ki ty, AsTmPair ki ty pt tm) => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferTmFst inferFn tm = do
  tmP <- preview _TmFst tm
  return $ do
    tyP <- inferFn tmP
    tyP1 <- fmap (review _TyVar) freshTyVar
    tyP2 <- fmap (review _TyVar) freshTyVar
    expectEq tyP (review _TyPair (tyP1, tyP2))
    -- would be nice to tag this with the expectTyPair error somehow
    -- ty1, _) <- expectTyPair tyP
    return tyP1

inferTmSnd :: (UnificationContext e m ki ty a, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsExpectedEq e ki ty a, AsExpectedTyPair e ki ty a, AsTyPair ki ty, AsTmPair ki ty pt tm) => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferTmSnd inferFn tm = do
  tmP <- preview _TmSnd tm
  return $ do
    tyP <- inferFn tmP
    tyP1 <- fmap (review _TyVar) freshTyVar
    tyP2 <- fmap (review _TyVar) freshTyVar
    expectEq tyP (review _TyPair (tyP1, tyP2))
    -- would be nice to tag this with the expectTyPair error somehow
    -- (_, ty2) <- expectTyPair tyP
    return tyP2

checkPair :: (UnificationContext e m ki ty a, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsExpectedTyPair e ki ty a, AsTyPair ki ty, AsPtPair pt)
          => (Pattern pt a -> Type ki ty a -> UnifyT ki ty a m [Type ki ty a])
          -> Pattern pt a
          -> Type ki ty a
          -> Maybe (UnifyT ki ty a m [Type ki ty a])
checkPair checkFn p ty = do
  (p1, p2) <- preview _PtPair p
  return $ do
    tyP1 <- fmap (review _TyVar) freshTyVar
    tyP2 <- fmap (review _TyVar) freshTyVar
    expectEq ty (review _TyPair (tyP1, tyP2))
    --(ty1, ty2) <- expectTyPair ty
    mappend <$> checkFn p1 tyP1 <*> checkFn p2 tyP2

type PairInferContext e w s r m ki ty pt tm a = (InferContext e w s r m ki ty pt tm a, UnificationContext e m ki ty a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyPair ki ty, AsExpectedTyPair e ki ty a, AsPtPair pt, AsTmPair ki ty pt tm)

pairInferRules :: PairInferContext e w s r m ki ty pt tm a
              => InferInput e w s r m ki ty pt tm a
pairInferRules =
  InferInput
    [ EquivRecurse equivPair ]
    [ UnificationMany unifyPair ]
    [ InferRecurse inferTmPair
    , InferRecurse inferTmFst
    , InferRecurse inferTmSnd
    ]
    [ PCheckRecurse checkPair ]
