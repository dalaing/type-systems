{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Fragment.Pair (
    TyFPair(..)
  , AsTyPair(..)
  , PtFPair(..)
  , AsPtPair(..)
  , TmFPair(..)
  , AsTmPair(..)
  , AsExpectedTyPair(..)
  , PairContext
  , pairFragmentLazy
  , pairFragmentStrict
  , tyPair
  , ptPair
  , tmPair
  , tmFst
  , tmSnd
  ) where

import Control.Monad.Except (MonadError)

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Bound
import Data.Deriving

import Fragment
import Fragment.Ast
import Util

data TyFPair f a =
  TyPairF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFPair

deriveEq1 ''TyFPair
deriveOrd1 ''TyFPair
deriveShow1 ''TyFPair

instance EqRec TyFPair where
  liftEqRec eR _ (TyPairF x1 y1) (TyPairF x2 y2) = eR x1 x2 && eR y1 y2

instance Bound TyFPair where
  TyPairF x y >>>= f = TyPairF (x >>= f) (y >>= f)

instance Bitransversable TyFPair where
  bitransverse fT fL (TyPairF x y) = TyPairF <$> fT fL x <*> fT fL y

class AsTyPair ty where
  _TyPairP :: Prism' (ty k a) (TyFPair k a)

  _TyPair :: Prism' (Type ty a) (Type ty a, Type ty a)
  _TyPair = _TyTree . _TyPairP . _TyPairF

instance AsTyPair TyFPair where
  _TyPairP = id

data PtFPair f a =
    PtPairF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFPair

deriveEq1 ''PtFPair
deriveOrd1 ''PtFPair
deriveShow1 ''PtFPair

instance Bound PtFPair where
  PtPairF x y >>>= f = PtPairF (x >>= f) (y >>= f)

instance Bitransversable PtFPair where
  bitransverse fT fL (PtPairF x y) = PtPairF <$> fT fL x <*> fT fL y

class AsPtPair pt where
  _PtPairP :: Prism' (pt k a) (PtFPair k a)

  _PtPair :: Prism' (Pattern pt a) (Pattern pt a, Pattern pt a)
  _PtPair = _PtTree . _PtPairP . _PtPairF

instance AsPtPair PtFPair where
  _PtPairP = id

data TmFPair (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmPairF (f a) (f a)
  | TmFstF (f a)
  | TmSndF (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFPair

deriveEq1 ''TmFPair
deriveOrd1 ''TmFPair
deriveShow1 ''TmFPair

instance Bound (TmFPair ty pt) where
  TmPairF x y >>>= f = TmPairF (x >>= f) (y >>= f)
  TmFstF x >>>= f = TmFstF (x >>= f)
  TmSndF x >>>= f = TmSndF (x >>= f)

instance Bitransversable (TmFPair ty pt) where
  bitransverse fT fL (TmPairF x y) = TmPairF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmFstF x) = TmFstF <$> fT fL x
  bitransverse fT fL (TmSndF x) = TmSndF <$> fT fL x

class AsTmPair ty pt tm where
  _TmPairP :: Prism' (tm ty pt k a) (TmFPair ty pt k a)

  _TmPair :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmPair = _Wrapped . _ATerm . _TmPairP . _TmPairF . bimapping _Unwrapped _Unwrapped

  _TmFst :: Prism' (Term ty pt tm a) (Term ty pt tm a)
  _TmFst = _Wrapped . _ATerm . _TmPairP . _TmFstF . _Unwrapped

  _TmSnd :: Prism' (Term ty pt tm a) (Term ty pt tm a)
  _TmSnd = _Wrapped . _ATerm . _TmPairP . _TmSndF . _Unwrapped

instance AsTmPair ty pt TmFPair where
  _TmPairP = id

-- Errors

class AsExpectedTyPair e ty a | e -> ty, e -> a where
  _ExpectedTyPair :: Prism' e (Type ty a)

expectTyPair :: (MonadError e m, AsExpectedTyPair e ty a, AsTyPair ty) => Type ty a -> m (Type ty a, Type ty a)
expectTyPair ty =
  case preview _TyPair ty of
    Just (ty1, ty2) -> return (ty1, ty2)
    _ -> throwing _ExpectedTyPair ty

-- Rules

stepFstLazy :: AsTmPair ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepFstLazy tm = do
  tmP <- preview _TmFst tm
  (tm1, _) <- preview _TmPair tmP
  return tm1

stepSndLazy :: AsTmPair ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepSndLazy tm = do
  tmP <- preview _TmSnd tm
  (_, tm2) <- preview _TmPair tmP
  return tm2

-- TODO check this, there might be more rules
evalRulesLazy :: AsTmPair ty pt tm => FragmentInput e s r m ty pt tm a
evalRulesLazy =
  FragmentInput [] [EvalBase stepFstLazy, EvalBase stepSndLazy] [] [] []

valuePair :: AsTmPair ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
valuePair valueFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  v1 <- valueFn tm1
  v2 <- valueFn tm2
  return $ review _TmPair (v1, v2)

stepFstStrict :: AsTmPair ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepFstStrict stepFn tm = do
  tmP <- preview _TmFst tm
  tmP' <- stepFn tmP
  return $ review _TmFst tmP'

stepSndStrict :: AsTmPair ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepSndStrict stepFn tm = do
  tmP <- preview _TmSnd tm
  tmP' <- stepFn tmP
  return $ review _TmSnd tmP'

stepElimFstStrict :: AsTmPair ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepElimFstStrict valueFn tm = do
  tmP <- preview _TmFst tm
  (tm1, tm2) <- preview _TmPair tmP
  v1 <- valueFn tm1
  _ <- valueFn tm2
  return v1

stepElimSndStrict :: AsTmPair ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepElimSndStrict valueFn tm = do
  tmP <- preview _TmSnd tm
  (tm1, tm2) <- preview _TmPair tmP
  _ <- valueFn tm1
  v2 <- valueFn tm2
  return v2

stepPair1 :: AsTmPair ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepPair1 stepFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  tm1' <- stepFn tm1
  return $ review _TmPair (tm1', tm2)

stepPair2 :: AsTmPair ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepPair2 valueFn stepFn tm = do
  (tm1, tm2) <- preview _TmPair tm
  v1 <- valueFn tm1
  tm2' <- stepFn tm2
  return $ review _TmPair (v1, tm2')

evalRulesStrict :: AsTmPair ty pt tm => FragmentInput e s r m ty pt tm a
evalRulesStrict =
  FragmentInput
    [ ValueRecurse valuePair ]
    [ EvalStep stepFstStrict
    , EvalStep stepSndStrict
    , EvalValue stepElimFstStrict
    , EvalValue stepElimSndStrict
    , EvalStep stepPair1
    , EvalValueStep stepPair2
    ]
    [] [] []

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

inferRules :: (MonadError e m, AsExpectedTyPair e ty a, AsTyPair ty, AsTmPair ty pt tm) => FragmentInput e s r m ty pt tm a
inferRules =
  FragmentInput
    [] []
    [ InferRecurse inferTmPair
    , InferRecurse inferTmFst
    , InferRecurse inferTmSnd
    ]
    [] []

matchPair :: (AsPtPair pt, AsTmPair ty pt tm) => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
matchPair matchFn p tm = do
  (p1, p2) <- preview _PtPair p
  (tm1, tm2) <- preview _TmPair tm
  tms1 <- matchFn p1 tm1
  tms2 <- matchFn p2 tm2
  return $ tms1 ++ tms2

checkPair :: (MonadError e m, AsExpectedTyPair e ty a, AsTyPair ty, AsPtPair pt) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkPair checkFn p ty = do
  (p1, p2) <- preview _PtPair p
  return $ do
    (ty1, ty2) <- expectTyPair ty
    mappend <$> checkFn p1 ty1 <*> checkFn p2 ty2

patternRules :: (MonadError e m, AsExpectedTyPair e ty a, AsTyPair ty, AsPtPair pt, AsTmPair ty pt tm) => FragmentInput e s r m ty pt tm a
patternRules =
  FragmentInput
    [] [] []
    [ PMatchRecurse matchPair ]
    [ PCheckRecurse checkPair ]

type PairContext e s r m ty pt tm a = (MonadError e m, AsExpectedTyPair e ty a, AsTyPair ty, AsPtPair pt, AsTmPair ty pt tm)

pairFragmentLazy :: PairContext e s r m ty pt tm a
             => FragmentInput e s r m ty pt tm a
pairFragmentLazy =
  evalRulesLazy `mappend` inferRules `mappend` patternRules

pairFragmentStrict :: PairContext e s r m ty pt tm a
             => FragmentInput e s r m ty pt tm a
pairFragmentStrict =
  evalRulesStrict `mappend` inferRules `mappend` patternRules

-- Helpers

tyPair :: AsTyPair ty => Type ty a -> Type ty a -> Type ty a
tyPair = curry $ review _TyPair

ptPair :: AsPtPair pt => Pattern pt a -> Pattern pt a -> Pattern pt a
ptPair = curry $ review _PtPair

tmPair :: AsTmPair ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmPair = curry $ review _TmPair

tmFst :: AsTmPair ty pt tm => Term ty pt tm a -> Term ty pt tm a
tmFst = review _TmFst

tmSnd :: AsTmPair ty pt tm => Term ty pt tm a -> Term ty pt tm a
tmSnd = review _TmSnd
