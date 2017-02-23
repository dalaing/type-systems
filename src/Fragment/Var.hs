{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Fragment.Var (
    PtFWild(..)
  , AsPtWild(..)
  , PtVarContext
  , ptVarFragment
  , TmVarContext
  , tmVarFragment
  , tyVar
  , ptVar
  , ptWild
  , tmVar
  ) where

import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens

import Bound
import Data.Deriving

import Fragment
import Fragment.Ast
import Util

data PtFWild (f :: * -> *) a =
    PtWildF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFWild

deriveEq1 ''PtFWild
deriveOrd1 ''PtFWild
deriveShow1 ''PtFWild

instance EqRec PtFWild where
  liftEqRec _ _ PtWildF PtWildF = True

instance OrdRec PtFWild where
  liftCompareRec _ _ PtWildF PtWildF = EQ

instance ShowRec PtFWild where
  liftShowsPrecRec _ _ _ _ n PtWildF = showsPrec n PtWildF

instance Bound PtFWild where
  PtWildF >>>= _ = PtWildF

instance Bitransversable PtFWild where
  bitransverse _ _ PtWildF = pure PtWildF

class AsPtWild pt where
  _PtWildP :: Prism' (pt k a) (PtFWild k a)

  _PtWild :: Prism' (Pattern pt a) ()
  _PtWild = _PtTree . _PtWildP . _PtWildF

instance AsPtWild PtFWild where
  _PtWildP = id

instance {-# OVERLAPPABLE #-} AsPtWild (TSum xs) => AsPtWild (TSum (x ': xs)) where
  _PtWildP = _TNext . _PtWildP

instance {-# OVERLAPPING #-} AsPtWild (TSum (PtFWild ': xs)) where
  _PtWildP = _TAdd . _PtWildP

matchWild :: AsPtWild pt => Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
matchWild p _ = do
  _ <- preview _PtWild p
  return []

checkWild :: (Monad m, AsPtWild pt) => Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkWild p _ = do
  _ <- preview _PtWild p
  return $
    return []

matchVar :: Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
matchVar p tm = do
  _ <- preview _PtVar p
  return [tm]

checkVar :: Monad m => Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkVar p ty = do
  _ <- preview _PtVar p
  return $
    return [ty]

type PtVarContext e s r m (ty :: (* -> *) -> * -> *) pt (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Monad m, AsPtWild pt)

ptVarFragment :: PtVarContext e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
ptVarFragment =
  FragmentInput
    []
    []
    []
    [ PMatchBase matchWild, PMatchBase matchVar ]
    [ PCheckBase checkWild, PCheckBase checkVar ]
    [ ]
    [ ]

-- Rules

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, HasTermContext r ty a a, AsUnboundTermVariable e a) => Term ty pt tm a -> Maybe (m (Type ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

type TmVarContext e s r m ty (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (Ord a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsUnboundTermVariable e a)

tmVarFragment :: TmVarContext e s r m ty pt tm a
            => FragmentInput e s r m ty pt tm a
tmVarFragment =
  FragmentInput
    [] [] [InferBase inferTmVar] [] [] [] []

-- Helpers

tyVar :: a -> Type ty a
tyVar = review _TyVar

ptVar :: a -> Pattern pt a
ptVar = review _PtVar

ptWild :: AsPtWild pt => Pattern pt a
ptWild = review _PtWild ()

tmVar :: a -> Term ty pt tm a
tmVar = review _TmVar
