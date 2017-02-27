{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Pair.Rules.Infer (
    PairInferContext
  , pairInferRules
  ) where

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Infer
import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Pair.Ast.Type
import Fragment.Pair.Ast.Error
import Fragment.Pair.Ast.Pattern
import Fragment.Pair.Ast.Term

equivPair :: AsTyPair ty => (Type ty a -> Type ty a -> Bool) -> Type ty a -> Type ty a -> Maybe Bool
equivPair equivFn ty1 ty2 = do
  (p1a, p1b) <- preview _TyPair ty1
  (p2a, p2b) <- preview _TyPair ty2
  return $ equivFn p1a p2a && equivFn p1b p2b

inferTmPair :: (Monad m, AsTyPair ty, AsTmPair ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmPair inferFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    return $ review _TyPair (ty1, ty2)

inferTmFst :: (MonadError e m, AsExpectedTyPair e ty a, AsTyPair ty, AsTmPair ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmFst inferFn tm = do
  tmP <- preview _TmFst tm
  return $ do
    tyP <- inferFn tmP
    (ty1, _) <- expectTyPair tyP
    return ty1

inferTmSnd :: (MonadError e m, AsExpectedTyPair e ty a, AsTyPair ty, AsTmPair ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmSnd inferFn tm = do
  tmP <- preview _TmFst tm
  return $ do
    tyP <- inferFn tmP
    (_, ty2) <- expectTyPair tyP
    return ty2

checkPair :: (MonadError e m, AsExpectedTyPair e ty a, AsTyPair ty, AsPtPair pt) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkPair checkFn p ty = do
  (p1, p2) <- preview _PtPair p
  return $ do
    (ty1, ty2) <- expectTyPair ty
    mappend <$> checkFn p1 ty1 <*> checkFn p2 ty2

type PairInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, AsTyPair ty, AsExpectedTyPair e ty a, AsPtPair pt, AsTmPair ty pt tm)

pairInferRules :: PairInferContext e w s r m ty pt tm a
              => InferInput e w s r m ty pt tm a
pairInferRules =
  InferInput
    [ EquivRecurse equivPair ]
    [ InferRecurse inferTmPair
    , InferRecurse inferTmFst
    , InferRecurse inferTmSnd
    ]
    [ PCheckRecurse checkPair ]
