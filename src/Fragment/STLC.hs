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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Fragment.STLC (
    TyFSTLC(..)
  , AsTySTLC(..)
  , TmFSTLC(..)
  , AsTmSTLC(..)
  , AsExpectedTyArr(..)
  , STLCContext
  , stlcFragmentLazy
  , stlcFragmentStrict
  , tyArr
  , tmLam
  , tmApp
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

data TyFSTLC f a =
  TyArrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveEq1 ''TyFSTLC
deriveOrd1 ''TyFSTLC
deriveShow1 ''TyFSTLC

makePrisms ''TyFSTLC

class AsTySTLC ty where
  _TySTLCP :: Prism' (ty k a) (TyFSTLC k a)

  _TyArr :: Prism' (Type ty a) (Type ty a, Type ty a)
  _TyArr = _TyTree . _TySTLCP . _TyArrF

instance AsTySTLC TyFSTLC where
  _TySTLCP = id

instance {-# OVERLAPPABLE #-} AsTySTLC (TSum xs) => AsTySTLC (TSum (x ': xs)) where
  _TySTLCP = _TNext . _TySTLCP

instance {-# OVERLAPPING #-} AsTySTLC (TSum (TyFSTLC ': xs)) where
  _TySTLCP = _TAdd . _TySTLCP

instance EqRec TyFSTLC where
  liftEqRec eR _ (TyArrF x1 y1) (TyArrF x2 y2) = eR x1 x2 && eR y1 y2

instance OrdRec TyFSTLC where
  liftCompareRec cR _ (TyArrF x1 y1) (TyArrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      x -> x

instance ShowRec TyFSTLC where
  liftShowsPrecRec sR _ _ _ n (TyArrF x y) =
    showsBinaryWith sR sR "TyArrF" n x y

instance Bound TyFSTLC where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)

instance Bitransversable TyFSTLC where
  bitransverse fT fL (TyArrF x y) = TyArrF <$> fT fL x <*> fT fL y

data TmFSTLC (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
    TmLamF (k a) (Scope () k a)
  | TmAppF (k a) (k a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFSTLC

instance (Eq1 k, Monad k) => Eq1 (TmFSTLC ty pt k) where
  liftEq = $(makeLiftEq ''TmFSTLC)

instance (Ord1 k,  Monad k) => Ord1 (TmFSTLC ty pt k) where
  liftCompare = $(makeLiftCompare ''TmFSTLC)

instance (Show1 k) => Show1 (TmFSTLC ty pt k) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFSTLC)

instance EqRec (TmFSTLC ty pt) where
  liftEqRec eR e (TmLamF ty1 s1) (TmLamF ty2 s2) =
    eR ty1 ty2 && liftEqRec eR e s1 s2
  liftEqRec eR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFSTLC ty pt) where
  liftCompareRec cR c (TmLamF ty1 s1) (TmLamF ty2 s2) =
    case cR ty1 ty2 of
      EQ -> liftCompareRec cR c s1 s2
      z -> z
  liftCompareRec _ _ (TmLamF _ _) _ = LT
  liftCompareRec _ _ _ (TmLamF _ _) = GT
  liftCompareRec cR _ (TmAppF x1 y1) (TmAppF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFSTLC ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLamF ty sc) =
    showsBinaryWith sR (liftShowsPrecRec sR slR s sl) "TmLamF" n ty sc
  liftShowsPrecRec sR _ _ _ n (TmAppF x y) =
    showsBinaryWith sR sR "TmAppF" n x y

instance Bound (TmFSTLC ty pt) where
  TmLamF ty s >>>= f = TmLamF (ty >>= f) (s >>>= f)
  TmAppF x y >>>= f = TmAppF (x >>= f) (y >>= f)

instance Bitransversable (TmFSTLC ty pt) where
  bitransverse fT fL (TmLamF ty s) = TmLamF <$> fT fL ty <*> bitransverse fT fL s
  bitransverse fT fL (TmAppF x y) = TmAppF <$> fT fL x <*> fT fL y

class AstTransversable ty pt tm => AsTmSTLC ty pt tm where
  _TmSTLCP :: Prism' (tm ty pt k a) (TmFSTLC ty pt k a)

  _TmSTLCLink :: Prism' (Term ty pt tm a) (TmFSTLC ty pt (Ast ty pt tm) (AstVar a))
  _TmSTLCLink = _Wrapped . _ATerm . _TmSTLCP

  _TmLam :: Prism' (Term ty pt tm a) (Type ty a, Scope () (Ast ty pt tm) (AstVar a))
  _TmLam = _TmSTLCLink . _TmLamF . mkPair _Type id

  _TmApp :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmApp = _TmSTLCLink . _TmAppF . mkPair _Unwrapped _Unwrapped

instance (Bitransversable ty, Bitransversable pt) => AsTmSTLC ty pt TmFSTLC where
  _TmSTLCP = id

-- Errors

class AsExpectedTyArr e ty a | e -> ty, e -> a where
  _ExpectedTyArr :: Prism' e (Type ty a)

expectTyArr :: (MonadError e m, AsExpectedTyArr e ty a, AsTySTLC ty) => Type ty a -> m (Type ty a, Type ty a)
expectTyArr ty =
  case preview _TyArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedTyArr ty

-- Rules

valTmLam :: AsTmSTLC ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
valTmLam tm = do
  _ <- preview _TmLam tm
  return  tm

stepTmApp1 :: AsTmSTLC ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmApp1 evalFn tm = do
  (f, x) <- preview _TmApp tm
  f' <- evalFn f
  return $ review _TmApp (f', x)

stepTmLamApp :: AstBound ty pt tm => AsTmSTLC ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmLamApp tm = do
  (tmF, tmX) <- preview _TmApp tm
  (_, s) <- preview _TmLam tmF
  return . review _Wrapped . instantiate1 (review _Unwrapped tmX) $ s

inferTmLam :: (Ord a, AstBound ty pt tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, AsTySTLC ty, AsTmSTLC ty pt tm, HasTermContext r ty a a) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmLam inferFn tm = do
  (tyArg, s) <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: (Eq a, EqRec ty, MonadError e m, AsTySTLC ty, AsTmSTLC ty pt tm, AsExpectedTyArr e ty a, AsExpectedEq e ty a) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectEq tyArg tyX
    return tyRet

type STLCContext e s r m ty pt tm a = (Ord a, EqRec ty, AstBound ty pt tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsExpectedEq e ty a, AsExpectedTyArr e ty a, AsTySTLC ty, AsTmSTLC ty pt tm)

stlcFragmentLazy :: STLCContext e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
stlcFragmentLazy =
  FragmentInput
    [ValueBase valTmLam]
    [ EvalStep stepTmApp1
    , EvalBase stepTmLamApp
    ]
    [ InferRecurse inferTmLam
    , InferRecurse inferTmApp
    ]
    [] [] [] []

stepTmApp2 :: AsTmSTLC ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTmApp2 valueFn stepFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  vF <- valueFn tmF
  tmX' <- stepFn tmX
  return $ review _TmApp (vF, tmX')

stlcFragmentStrict :: STLCContext e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
stlcFragmentStrict =
  mappend stlcFragmentLazy $ FragmentInput [] [EvalValueStep stepTmApp2] [] [] [] [] []

-- Helpers

tyArr :: AsTySTLC ty => Type ty a -> Type ty a -> Type ty a
tyArr = curry $ review _TyArr

tmLam :: (Eq a, AsTmSTLC ty pt tm, Bound ty, Bound pt, Bound (tm ty pt)) => a -> Type ty a -> Term ty pt tm a -> Term ty pt tm a
tmLam v ty tm = review _TmLam (ty, abstract1 (review _ATmVar v) . review _Unwrapped $ tm)

tmApp :: AsTmSTLC ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmApp = curry $ review _TmApp
