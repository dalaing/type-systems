{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Pair.Rules.Type.Infer.Offline (
    PairInferTypeContext
  , pairInferTypeRules
  ) where

import Control.Monad.State (MonadState)
import Control.Lens (review, preview)
import Data.Equivalence.Monad (EquivT, classDesc)

import Rules.Unification
import Rules.Type.Infer.Offline
import Ast.Type
import Ast.Type.Var
import Ast.Error.Common
import Data.Functor.Rec

import Fragment.Pair.Ast.Type
import Fragment.Pair.Ast.Error
import Fragment.Pair.Ast.Pattern
import Fragment.Pair.Ast.Term

import Fragment.Pair.Rules.Type.Infer.Common

unifyPair :: (UnificationContext e m (Type ki ty) a, AsTyPair ki ty)
          => ([Type ki ty a] -> [Type ki ty a] -> EquivT s (Type ki ty a) (Type ki ty a) m ())
          -> UConstraint (Type ki ty) a
          -> Maybe (EquivT s (Type ki ty a) (Type ki ty a) m ())
unifyPair unifyMany (UCEq ty1 ty2) = do
  (p1a, p1b) <- preview _TyPair ty1
  (p2a, p2b) <- preview _TyPair ty2
  return $ do
    c1a <- classDesc p1a
    c1b <- classDesc p1b
    c2a <- classDesc p2a
    c2b <- classDesc p2b
    unifyMany [c1a, c1b] [c2a, c2b]

createPair :: (Eq a, EqRec (ty ki), MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyPair ki ty) => Type ki ty a -> Type ki ty a -> UnifyT ki ty a m (Type ki ty a)
createPair ty1 ty2 = do
  tyV <- fmap (review _TyVar) freshTyVar
  expectTypeEq (review _TyPair (ty1, ty2)) tyV
  return tyV

expectPair :: (Eq a, EqRec (ty ki), MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyPair ki ty) => Type ki ty a -> UnifyT ki ty a m (Type ki ty a, Type ki ty a)
expectPair tyP = do
  tyP1 <- fmap (review _TyVar) freshTyVar
  tyP2 <- fmap (review _TyVar) freshTyVar
  expectTypeEq tyP (review _TyPair (tyP1, tyP2))
  return (tyP1, tyP2)

type PairInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, UnificationContext e m (Type ki ty) a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsTyPair ki ty, AsExpectedTypeEq e ki ty a, AsExpectedTyPair e ki ty a, AsPtPair pt, AsTmPair ki ty pt tm)

pairInferTypeRules :: PairInferTypeContext e w s r m ki ty pt tm a
              => InferTypeInput e w s r m ki ty pt tm a
pairInferTypeRules =
  let
    ph = PairHelper createPair expectPair
  in
    InferTypeInput
      [ UnificationMany unifyPair ]
      (inferTypeRules ph)
      (checkRules ph)

