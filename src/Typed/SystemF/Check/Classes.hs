{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Typed.SystemF.Check.Classes (
    HasTmVarSupply(..)
  , ToTmVar(..)
  , freshTmVar
  , HasTyVarSupply(..)
  , ToTyVar(..)
  , freshTyVar
  , TermContext
  , emptyTermContext
  , HasTermContext(..)
  , AsUnboundTermVariable(..)
  , lookupTerm
  , insertTerm
  , AsUnexpected(..)
  , expect
  , AsExpectedEq(..)
  , expectEq
  , AsExpectedTyArr(..)
  , expectTyArr
  , AsExpectedTyAll(..)
  , expectTyAll
  , AsUnknownTypeError(..)
  ) where

import Control.Monad (unless)

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Typed.SystemF.Type.Class

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

class HasTyVarSupply s where
  tyVarSupply :: Lens' s Int

class ToTyVar a where
  toTyVar :: Int -> a

instance ToTyVar T.Text where
  toTyVar x = T.append "X" (T.pack . show $ x)

freshTyVar :: (MonadState s m, HasTyVarSupply s, ToTyVar a) => m a
freshTyVar = do
  x <- use tyVarSupply
  tyVarSupply %= succ
  return $ toTyVar x

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

class AsUnexpected e ty | e -> ty where
  _Unexpected :: Prism' e (ty, ty)

expect :: (Eq ty, MonadError e m, AsUnexpected e ty) => ty -> ty -> m ()
expect ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _Unexpected (ty1, ty2)

class AsExpectedEq e ty | e -> ty where
  _ExpectedEq :: Prism' e (ty, ty)

expectEq :: (Eq ty, MonadError e m, AsExpectedEq e ty) => ty -> ty -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _ExpectedEq (ty1, ty2)

class AsExpectedTyArr e ty | e -> ty where
  _ExpectedTyArr :: Prism' e ty

expectTyArr :: (MonadError e m, AsExpectedTyArr e (ty a), AsType ty) => ty a -> m (ty a, ty a)
expectTyArr ty =
  case preview _TyArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedTyArr ty

class AsExpectedTyAll e ty | e -> ty where
  _ExpectedTyAll :: Prism' e ty

expectTyAll :: (Eq a, MonadError e m, MonadState s m, HasTyVarSupply s, ToTyVar a, AsExpectedTyAll e (ty a), AsType ty) => ty a -> m (a, ty a)
expectTyAll ty = do
  x <- freshTyVar
  case preview _TyAll (x, ty) of
    Just (v, tyA) -> return (v, tyA)
    _ -> throwing _ExpectedTyAll ty

class AsUnknownTypeError e where
  _UnknownTypeError :: Prism' e ()

