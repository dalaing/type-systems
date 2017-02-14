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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Var (
    PtFVar(..)
  , AsPtVar(..)
  , PtVarContext
  , ptVarFragment
  , AsTmVar(..)
  , HasTmVarSupply(..)
  , ToTmVar(..)
  , freshTmVar
  , tmVar
  , TermContext(..)
  , emptyTermContext
  , HasTermContext(..)
  , AsUnboundTermVariable(..)
  , lookupTerm
  , insertTerm
  , TmVarContext
  , tmVarFragment
  ) where

import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens

import Fragment

data PtFVar (f :: * -> *) a =
    PtWildF
  | PtVarF a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFVar

class AsPtVar pt where
  _PtVarP :: Prism' (pt a) (PtFVar pt a)

  _PtWild :: Prism' (pt a) ()
  _PtWild = _PtVarP . _PtWildF

  _PtVar :: Prism' (pt a) a
  _PtVar = _PtVarP . _PtVarF

instance AsPtVar pt => AsPtVar (PtFVar pt) where
  _PtVarP = id . _PtVarP

matchWild :: AsPtVar p => p a -> tm a -> Maybe [tm a]
matchWild p _ = do
  _ <- preview _PtWild p
  return []

matchVar :: AsPtVar p => p a -> tm a -> Maybe [tm a]
matchVar p tm = do
  _ <- preview _PtVar p
  return [tm]

checkWild :: (Monad m, AsPtVar p) => p a -> ty a -> Maybe (m [ty a])
checkWild p _ = do
  _ <- preview _PtWild p
  return $
    return []

checkVar :: (Monad m, AsPtVar p) => p a -> ty a -> Maybe (m [ty a])
checkVar p ty = do
  _ <- preview _PtVar p
  return $
    return [ty]

type PtVarContext e s r m (ty :: * -> *) p (tm :: * -> *) a = (Monad m, AsPtVar p)

ptVarFragment :: (Monad m, AsPtVar p) => FragmentInput e s r m ty p tm a
ptVarFragment =
  FragmentInput
    []
    []
    []
    [ PMatchBase matchWild
    , PMatchBase matchVar
    ]
    [ PCheckBase checkWild
    , PCheckBase checkVar
    ]

-- Possibly _TmVar :: Prism' tm a, so we can use it with
-- Term (ASTar a) and pull an a out
class AsTmVar tm where
  _TmVar :: Prism' (tm a) a

-- State

class HasTmVarSupply s where
  tmVarSupply :: Lens' s Int

instance HasTmVarSupply Int where
  tmVarSupply = id

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

-- Rules

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, AsTmVar tm, HasTermContext r ty a a, AsUnboundTermVariable e a) => tm a -> Maybe (m (ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

type TmVarContext e s r m ty (p :: * -> *) tm a = (Ord a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsUnboundTermVariable e a, AsTmVar tm)

tmVarFragment :: TmVarContext e s r m ty p tm a
            => FragmentInput e s r m ty p tm a
tmVarFragment =
  FragmentInput
    [] [] [InferBase inferTmVar] [] []

-- Helpers

tmVar :: AsTmVar tm => a -> tm a
tmVar = review _TmVar
