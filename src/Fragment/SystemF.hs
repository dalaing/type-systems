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
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Fragment.SystemF (
    TyFSystemF(..)
  , AsTySystemF(..)
  , TmFSystemF(..)
  , AsTmSystemF(..)
  , AsExpectedTyArr(..)
  , AsExpectedTyAll(..)
  , SystemFContext
  , systemFFragmentLazy
  , systemFFragmentStrict
  , tyArr
  , tyAll
  , tmLam
  , tmApp
  , tmLamTy
  , tmAppTy
  ) where

import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)

import Control.Monad.Error.Lens (throwing)

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Fragment.Ast
import Fragment.Var
import Error
import Util

data TyFSystemF f a =
    TyArrF (f a) (f a)
  | TyAllF (Scope () f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFSystemF

instance (Eq1 k, Monad k) => Eq1 (TyFSystemF k) where
  liftEq = $(makeLiftEq ''TyFSystemF)

instance (Ord1 k, Monad k) => Ord1 (TyFSystemF k) where
  liftCompare = $(makeLiftCompare ''TyFSystemF)

instance (Show1 k) => Show1 (TyFSystemF k) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFSystemF)

class AsTySystemF ty where
  _TySystemFP :: Prism' (ty k a) (TyFSystemF k a)

  _TyArr :: Prism' (Type ty a) (Type ty a, Type ty a)
  _TyArr = _TyTree . _TySystemFP . _TyArrF

  _TyAll :: Prism' (Type ty a) (Scope () (Type ty) a)
  _TyAll = _TyTree . _TySystemFP . _TyAllF

instance AsTySystemF TyFSystemF where
  _TySystemFP = id

instance EqRec TyFSystemF where
  liftEqRec eR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec eR e (TyAllF s1) (TyAllF s2) =
    liftEqRec eR e s1 s2

instance OrdRec TyFSystemF where
  liftCompareRec cR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x
  liftCompareRec _ _ (TyArrF _ _) _ = LT
  liftCompareRec _ _ _ (TyArrF _ _) = GT
  liftCompareRec cR c (TyAllF s1) (TyAllF s2) =
    liftCompareRec cR c s1 s2
  liftCompareRec _ _ (TyAllF _) _ = LT
  liftCompareRec _ _ _ (TyAllF _) = GT

instance ShowRec TyFSystemF where
  liftShowsPrecRec sR _ _ _ n (TyArrF x y) =
    showsBinaryWith sR sR "TyArrF" n x y
  liftShowsPrecRec sR slR s sl n (TyAllF sc) =
    showsUnaryWith (liftShowsPrecRec sR slR s sl) "TyAllF" n sc

instance Bound TyFSystemF where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)
  TyAllF s >>>= f = TyAllF (s >>>= f)

instance Bitransversable TyFSystemF where
  bitransverse fT fL (TyArrF x y) = TyArrF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TyAllF s) = TyAllF <$> bitransverse fT fL s

data TmFSystemF (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmLamF (k a) (Scope () k a)
  | TmAppF (k a) (k a)
  | TmLamTyF (Scope () k a)
  | TmAppTyF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFSystemF

instance (Eq1 k, Monad k) => Eq1 (TmFSystemF ty pt k) where
  liftEq = $(makeLiftEq ''TmFSystemF)

instance (Ord1 k,  Monad k) => Ord1 (TmFSystemF ty pt k) where
  liftCompare = $(makeLiftCompare ''TmFSystemF)

instance (Show1 k) => Show1 (TmFSystemF ty pt k) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFSystemF)

class (TripleConstraint1 Traversable ty pt tm, Bitransversable ty, Traversable (ty (Type ty))) => AsTmSystemF ty pt tm where
  _TmSystemFP :: Prism' (tm ty pt k a) (TmFSystemF ty pt k a)

  _TmSystemFLink :: Prism' (Term ty pt tm a) (TmFSystemF ty pt (Ast ty pt tm) (AstVar a))
  _TmSystemFLink = _Wrapped . _ATerm . _TmSystemFP

  _TmLam :: Prism' (Term ty pt tm a) (Type ty a, Scope () (Ast ty pt tm) (AstVar a))
  _TmLam = _TmSystemFLink . _TmLamF . mkPair _Type id

  _TmApp :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmApp = _TmSystemFLink . _TmAppF . mkPair _Unwrapped _Unwrapped

  _TmLamTy :: Prism' (Term ty pt tm a) (Scope () (Ast ty pt tm) (AstVar a))
  _TmLamTy = _TmSystemFLink . _TmLamTyF

  _TmAppTy :: Prism' (Term ty pt tm a) (Term ty pt tm a, Type ty a)
  _TmAppTy = _TmSystemFLink . _TmAppTyF . mkPair _Unwrapped _Type

instance (TripleConstraint1 Traversable ty pt TmFSystemF, Bitransversable ty, Traversable (ty (Type ty))) => AsTmSystemF ty pt TmFSystemF where
  _TmSystemFP = id

instance Bound (TmFSystemF ty pt) where
  TmLamF ty s >>>= f = TmLamF (ty >>= f) (s >>>= f)
  TmAppF x y >>>= f = TmAppF (x >>= f) (y >>= f)
  TmLamTyF s >>>= f = TmLamTyF (s >>>= f)
  TmAppTyF x y >>>= f = TmAppTyF (x >>= f) (y >>= f)

instance Bitransversable (TmFSystemF ty pt) where
  bitransverse fT fL (TmLamF ty s) = TmLamF <$> fT fL ty <*> bitransverse fT fL s
  bitransverse fT fL (TmAppF x y) = TmAppF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmLamTyF s) = TmLamTyF <$> bitransverse fT fL s
  bitransverse fT fL (TmAppTyF x y) = TmAppTyF <$> fT fL x <*> fT fL y

-- Errors

class AsExpectedTyArr e ty a | e -> ty, e -> a where
  _ExpectedTyArr :: Prism' e (Type ty a)

expectTyArr :: (MonadError e m, AsExpectedTyArr e ty a, AsTySystemF ty) => Type ty a -> m (Type ty a, Type ty a)
expectTyArr ty =
  case preview _TyArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedTyArr ty

class AsExpectedTyAll e ty a | e -> ty, e -> a where
  _ExpectedTyAll :: Prism' e (Type ty a)

expectTyAll :: (MonadError e m, AsExpectedTyAll e ty a, AsTySystemF ty) => Type ty a -> m (Scope () (Type ty) a)
expectTyAll ty =
  case preview _TyAll ty of
    Just s -> return s
    _ -> throwing _ExpectedTyAll ty

-- Rules

valTmLam :: AsTmSystemF ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
valTmLam tm = do
  _ <- preview _TmLam tm
  return  tm

valTmLamTy :: AsTmSystemF ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
valTmLamTy tm = do
  _ <- preview _TmLamTy tm
  return  tm

stepTmApp1 :: AsTmSystemF ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmApp1 evalFn tm = do
  (f, x) <- preview _TmApp tm
  f' <- evalFn f
  return $ review _TmApp (f', x)

stepTmAppTy1 :: AsTmSystemF ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmAppTy1 evalFn tm = do
  (f, x) <- preview _TmAppTy tm
  f' <- evalFn f
  return $ review _TmAppTy (f', x)

stepTmLamApp :: (Bound ty, Bound pt, Bound (tm ty pt)) => AsTmSystemF ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmLamApp tm = do
  (tmF, tmX) <- preview _TmApp tm
  (_, s) <- preview _TmLam tmF
  return . review _Wrapped . instantiate1 (review _Unwrapped tmX) $ s

stepTmLamTyAppTy :: (Bound ty, Bound pt, Bound (tm ty pt)) => AsTmSystemF ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmLamTyAppTy tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  s <- preview _TmLamTy tmF
  return . review _Wrapped . instantiate1 (review _Type tyX) $ s

inferTmLam :: (Ord a, Bound ty, Bound pt, Bound (tm ty pt), MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, AsTySystemF ty, AsTmSystemF ty pt tm, HasTermContext r ty a a) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmLam inferFn tm = do
  (tyArg, s) <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: (Eq a, EqRec ty, MonadError e m, AsTySystemF ty, AsTmSystemF ty pt tm, AsExpectedTyArr e ty a, AsExpectedEq e ty a) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectEq tyArg tyX
    return tyRet

inferTmLamTy :: (Eq a, Bound ty, Bound pt, Bound (tm ty pt), MonadState s m, HasTyVarSupply s, ToTyVar a, AsTySystemF ty, AsTmSystemF ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmLamTy inferFn tm = do
  tmF <- preview _TmLamTy tm
  return $ do
    v <- freshTyVar
    ty <- inferFn (review _Wrapped . instantiate1 (review (_AVar . _ATyVar) v) $ tmF)
    return . review _TyAll . abstract1 v $ ty

inferTmAppTy :: (Bound ty, MonadError e m, AsExpectedTyAll e ty a , AsTySystemF ty, AsTmSystemF ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmAppTy inferFn tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  return $ do
    tyF <- inferFn tmF
    s <- expectTyAll tyF
    return $ instantiate1 tyX s

type SystemFContext e s r m ty pt tm a = (Ord a, EqRec ty, Bound ty, Bound pt, Bound (tm ty pt), MonadState s m, HasTmVarSupply s, ToTmVar a, HasTyVarSupply s, ToTyVar a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsExpectedEq e ty a, AsExpectedTyArr e ty a, AsExpectedTyAll e ty a, AsTySystemF ty, AsTmSystemF ty pt tm)

systemFFragmentLazy :: SystemFContext e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
systemFFragmentLazy =
  FragmentInput
    [ ValueBase valTmLam
    , ValueBase valTmLamTy
    ]
    [ EvalStep stepTmApp1
    , EvalStep stepTmAppTy1
    , EvalBase stepTmLamApp
    , EvalBase stepTmLamTyAppTy
    ]
    [ InferRecurse inferTmLam
    , InferRecurse inferTmApp
    , InferRecurse inferTmLamTy
    , InferRecurse inferTmAppTy
    ]
    [] []

stepTmApp2 :: AsTmSystemF ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmApp2 valueFn stepFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  vF <- valueFn tmF
  tmX' <- stepFn tmX
  return $ review _TmApp (vF, tmX')

systemFFragmentStrict :: SystemFContext e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
systemFFragmentStrict =
  mappend systemFFragmentLazy $ FragmentInput [] [EvalValueStep stepTmApp2] [] [] []

-- Helpers

tyArr :: AsTySystemF ty => Type ty a -> Type ty a -> Type ty a
tyArr = curry $ review _TyArr

tyAll :: (Eq a, Bound ty, Bitransversable ty, AsTySystemF ty) => a -> Type ty a -> Type ty a
tyAll v ty = review _TyAll (abstract1 v ty)

tmLam :: (Eq a, AsTmSystemF ty pt tm, Bound ty, Bound pt, Bound (tm ty pt)) => a -> Type ty a -> Term ty pt tm a -> Term ty pt tm a
tmLam v ty tm = review _TmLam (ty, abstract1 (review _ATmVar v) . review _Unwrapped $ tm)

tmApp :: AsTmSystemF ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmApp = curry $ review _TmApp

tmLamTy :: (Eq a, AsTmSystemF ty pt tm, Bound ty, Bound pt, Bound (tm ty pt)) => a -> Term ty pt tm a -> Term ty pt tm a
tmLamTy v tm = review _TmLamTy (abstract1 (review _ATyVar v) . review _Unwrapped $ tm)

tmAppTy :: AsTmSystemF ty pt tm => Term ty pt tm a -> Type ty a -> Term ty pt tm a
tmAppTy = curry $ review _TmAppTy
