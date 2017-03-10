{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Pair.Rules.Type.Infer.Common (
    PairHelper(..)
  , inferTypeInput
  ) where

import Control.Lens (preview)

import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Pair.Ast.Type
import Fragment.Pair.Ast.Pattern
import Fragment.Pair.Ast.Term

import Rules.Type.Infer.Common

data PairHelper m ki ty a =
  PairHelper {
    phCreatePair :: Type ki ty a -> Type ki ty a -> m (Type ki ty a)
  , phExpectPair :: Type ki ty a -> m (Type ki ty a, Type ki ty a)
  }

inferTypeInput :: (Monad mi, AsTyPair ki ty, AsPtPair pt, AsTmPair ki ty pt tm)
               => PairHelper mi ki ty a
               -> InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput ph =
  InferTypeInput
    []
    [ InferTypeRecurse $ inferTmPair ph
    , InferTypeRecurse $ inferTmFst ph
    , InferTypeRecurse $ inferTmSnd ph
    ]
    [ PCheckRecurse $ checkPair ph]

inferTmPair :: (Monad m, AsTyPair ki ty, AsTmPair ki ty pt tm)
            => PairHelper m ki ty a
            -> (Term ki ty pt tm a -> m (Type ki ty a))
            -> Term ki ty pt tm a
            -> Maybe (m (Type ki ty a))
inferTmPair (PairHelper createPairFn _) inferFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  return $ do
    ty1 <- inferFn tm1
    ty2 <- inferFn tm2
    createPairFn ty1 ty2

inferTmFst :: (Monad m, AsTyPair ki ty, AsTmPair ki ty pt tm)
           => PairHelper m ki ty a
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmFst (PairHelper _ expectPairFn) inferFn tm = do
  tmP <- preview _TmFst tm
  return $ do
    tyP <- inferFn tmP
    (ty1, _) <- expectPairFn tyP
    return ty1

inferTmSnd :: (Monad m, AsTyPair ki ty, AsTmPair ki ty pt tm)
           => PairHelper m ki ty a
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmSnd (PairHelper _ expectPairFn) inferFn tm = do
  tmP <- preview _TmFst tm
  return $ do
    tyP <- inferFn tmP
    (_, ty2) <- expectPairFn tyP
    return ty2

checkPair :: (Monad m, AsTyPair ki ty, AsPtPair pt)
          => PairHelper m ki ty a
          -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a])
          -> Pattern pt a
          -> Type ki ty a
          -> Maybe (m [Type ki ty a])
checkPair (PairHelper _ expectPairFn) checkFn p ty = do
  (p1, p2) <- preview _PtPair p
  return $ do
    (ty1, ty2) <- expectPairFn ty
    mappend <$> checkFn p1 ty1 <*> checkFn p2 ty2
