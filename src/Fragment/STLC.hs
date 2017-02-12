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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Fragment.STLC (
    TyFSTLC(..)
  , AsTySTLC(..)
  , TmFSTLC(..)
  , AsTmSTLC(..)
  , AsTmVar(..)
  , stlcFragmentLazy
  , stlcFragmentStrict
  , tyArr
  , tmVar
  , tmLam
  , tmApp
  ) where

import Data.Void

import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)

import Control.Monad.Error.Lens (throwing)

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Error

data TyFSTLC f a =
  TyArrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFSTLC

instance Eq1 f => Eq1 (TyFSTLC f) where
  liftEq = $(makeLiftEq ''TyFSTLC)

instance Ord1 f => Ord1 (TyFSTLC f) where
  liftCompare = $(makeLiftCompare ''TyFSTLC)

instance Show1 f => Show1 (TyFSTLC f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TyFSTLC)

instance Bound TyFSTLC where
  TyArrF x y >>>= f = TyArrF (x >>= f) (y >>= f)

data TmFSTLC ty tyV tm tmV =
    TmLamF (ty tyV) (Scope () tm tmV)
  | TmAppF (tm tmV) (tm tmV)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFSTLC

instance (Eq (ty tyV), Eq1 tm, Monad tm) => Eq1 (TmFSTLC ty tyV tm) where
  liftEq = $(makeLiftEq ''TmFSTLC)

instance (Ord (ty tyV), Ord1 tm, Monad tm) => Ord1 (TmFSTLC ty tyV tm) where
  liftCompare = $(makeLiftCompare ''TmFSTLC)

instance (Show (ty tyV), Show1 tm) => Show1 (TmFSTLC ty tyV tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFSTLC)

instance Bound (TmFSTLC ty Void) where
  TmLamF ty s >>>= f = TmLamF ty (s >>>= f)
  TmAppF x y >>>= f = TmAppF (x >>= f) (y >>= f)

class AsTySTLC ty where
  _TySTLCP :: Prism' (ty a) (TyFSTLC ty a)

  _TyArr :: Prism' (ty a) (ty a, ty a)
  _TyArr = _TySTLCP . _TyArrF

instance AsTySTLC f => AsTySTLC (TyFSTLC f) where
  _TySTLCP = id . _TySTLCP

-- Possibly _TmVar :: Prism' tm a, so we can use it with
-- Term (ASTar a) and pull an a out
class AsTmVar tm where
  _TmVar :: Prism' (tm a) a

class AsTmSTLC ty tm | tm -> ty where
  _TmSTLCP :: Prism' (tm a) (TmFSTLC ty a tm a)

  _TmLam :: Prism' (tm a) (ty a, Scope () tm a)
  _TmLam = _TmSTLCP . _TmLamF

  _TmApp :: Prism' (tm a) (tm a, tm a)
  _TmApp = _TmSTLCP . _TmAppF

-- State

class HasTmVarSupply s where
  tmVarSupply :: Lens' s Int

class ToTmVar a where
  toTmVar :: Int -> a

instance ToTmVar T.Text where
  toTmVar x = T.append "x" (T.pack . show $ x)

freshTmVar :: (MonadState s m, HasTmVarSupply s, ToTmVar a) => m a
freshTmVar = do
  x <- use tmVarSupply
  tmVarSupply %= succ
  return $ toTmVar x

-- Context

data TermContext ty tmV tyV = TermContext (M.Map tmV (ty tyV))

emptyTermContext :: TermContext ty tmV tyV
emptyTermContext = TermContext M.empty

instance HasTermContext (TermContext ty tmV tyV) ty tmV tyV where
  termContext = id

class HasTermContext l ty tmV tyV | l -> ty, l -> tmV, l -> tyV where
  termContext :: Lens' l (TermContext ty tmV tyV)

class AsUnboundTermVariable e tm | e -> tm where
  _UnboundTermVariable :: Prism' e tm

lookupTerm :: (Ord tmV, MonadReader r m, MonadError e m, HasTermContext r ty tmV tyV, AsUnboundTermVariable e tmV) => tmV -> m (ty tyV)
lookupTerm v = do
  TermContext m <- view termContext
  case M.lookup v m of
    Nothing -> throwing _UnboundTermVariable v
    Just ty -> return ty

insertTerm :: Ord tmV => tmV -> ty tyV -> TermContext ty tmV tyV -> TermContext ty tmV tyV
insertTerm v ty (TermContext m) = TermContext (M.insert v ty m)

-- Errors

class AsExpectedTyArr e ty | e -> ty where
  _ExpectedTyArr :: Prism' e ty

expectTyArr :: (MonadError e m, AsExpectedTyArr e (ty a), AsTySTLC ty) => ty a -> m (ty a, ty a)
expectTyArr ty =
  case preview _TyArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedTyArr ty

-- Rules

valTmLam :: AsTmSTLC ty tm => tm a -> Maybe (tm a)
valTmLam tm = do
  _ <- preview _TmLam tm
  return  tm

stepTmApp1 :: AsTmSTLC ty tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepTmApp1 evalFn tm = do
  (f, x) <- preview _TmApp tm
  f' <- evalFn f
  return $ review _TmApp (f', x)

stepTmLamApp :: (Monad tm, AsTmSTLC ty tm) => tm a -> Maybe (tm a)
stepTmLamApp tm = do
  (f, x) <- preview _TmApp tm
  (_, s) <- preview _TmLam f
  return $ instantiate1 x s

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, AsTmVar tm, HasTermContext r ty a a, AsUnboundTermVariable e a) => tm a -> Maybe (m (ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

inferTmLam :: (Ord a, Monad tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, AsTySTLC ty, AsTmVar tm, AsTmSTLC ty tm, HasTermContext r ty a a) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmLam inferFn tm = do
  (tyArg, s) <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    let tmF = instantiate1 (review _TmVar v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: (Eq (ty a), MonadError e m, AsTySTLC ty, AsTmSTLC ty tm, AsExpectedTyArr e (ty a), AsExpectedEq e (ty a)) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectEq tyArg tyX
    return tyRet

stlcFragmentLazy :: (Eq (ty a), Ord a, Monad tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsExpectedEq e (ty a), AsExpectedTyArr e (ty a), AsUnboundTermVariable e a, AsTySTLC ty, AsTmVar tm, AsTmSTLC ty tm)
            => FragmentInput e s r m ty tm a
stlcFragmentLazy =
  FragmentInput
    [ValueBase valTmLam]
    [ EvalStep stepTmApp1
    , EvalBase stepTmLamApp
    ]
    [ InferBase inferTmVar
    , InferRecurse inferTmLam
    , InferRecurse inferTmApp
    ]

stepTmApp2 :: AsTmSTLC ty tm => (tm a -> Maybe (tm a)) -> (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
stepTmApp2 valueFn stepFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  vF <- valueFn tmF
  tmX' <- stepFn tmX
  return $ review _TmApp (vF, tmX')

stlcFragmentStrict :: (Eq (ty a), Ord a, Monad tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsExpectedEq e (ty a), AsExpectedTyArr e (ty a), AsUnboundTermVariable e a, AsTySTLC ty, AsTmVar tm, AsTmSTLC ty tm)
            => FragmentInput e s r m ty tm a
stlcFragmentStrict =
  mappend stlcFragmentLazy $ FragmentInput [] [EvalValueStep stepTmApp2] []

-- Helpers

tyArr :: AsTySTLC ty => ty a -> ty a -> ty a
tyArr = curry $ review _TyArr

tmVar :: AsTmVar tm => a -> tm a
tmVar = review _TmVar

tmLam :: (Eq a, Monad tm, AsTmSTLC ty tm) => a -> ty a -> tm a -> tm a
tmLam v ty tm = review _TmLam (ty, abstract1 v tm)

tmApp :: AsTmSTLC ty tm => tm a -> tm a -> tm a
tmApp = curry $ review _TmApp
