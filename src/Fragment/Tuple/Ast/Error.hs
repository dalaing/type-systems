{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Fragment.Tuple.Ast.Error (
    ErrExpectedTyTuple(..)
  , AsExpectedTyTuple(..)
  , expectTyTuple
  , ErrTupleOutOfBounds(..)
  , AsTupleOutOfBounds(..)
  , lookupTuple
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens (preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Type
import Ast.Error

import Fragment.Tuple.Ast.Type

data ErrExpectedTyTuple (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrExpectedTyTuple (Type ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyTuple

class AsExpectedTyTuple e ty a | e -> ty, e -> a where
  _ExpectedTyTuple :: Prism' e (Type ty a)

instance AsExpectedTyTuple (ErrExpectedTyTuple ty pt tm a) ty a where
  _ExpectedTyTuple = _ErrExpectedTyTuple

instance {-# OVERLAPPABLE #-} AsExpectedTyTuple ((ErrSum xs) ty pt tm a) ty a => AsExpectedTyTuple (ErrSum (x ': xs) ty pt tm a) ty a where
  _ExpectedTyTuple = _ErrNext . _ExpectedTyTuple

instance {-# OVERLAPPING #-} AsExpectedTyTuple (ErrSum (ErrExpectedTyTuple ': xs) ty pt tm a) ty a where
  _ExpectedTyTuple = _ErrNow . _ExpectedTyTuple

expectTyTuple :: (MonadError e m, AsExpectedTyTuple e ty a, AsTyTuple ty) => Type ty a -> m [Type ty a]
expectTyTuple ty =
  case preview _TyTuple ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyTuple ty

data ErrTupleOutOfBounds (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrTupleOutOfBounds Int Int
  deriving (Eq, Ord, Show)

makePrisms ''ErrTupleOutOfBounds

class AsTupleOutOfBounds e where
  _TupleOutOfBounds :: Prism' e (Int, Int)

instance AsTupleOutOfBounds (ErrTupleOutOfBounds ty pt tm a) where
  _TupleOutOfBounds = _ErrTupleOutOfBounds

instance {-# OVERLAPPABLE #-} AsTupleOutOfBounds ((ErrSum xs) ty pt tm a) => AsTupleOutOfBounds (ErrSum (x ': xs) ty pt tm a) where
  _TupleOutOfBounds = _ErrNext . _TupleOutOfBounds

instance {-# OVERLAPPING #-} AsTupleOutOfBounds (ErrSum (ErrTupleOutOfBounds ': xs) ty pt tm a) where
  _TupleOutOfBounds = _ErrNow . _TupleOutOfBounds

lookupTuple :: (MonadError e m, AsTupleOutOfBounds e) =>  [t a] -> Int -> m (t a)
lookupTuple ts i =
  let
    l = length ts
    f x
      | x < 0 = throwing _TupleOutOfBounds (x, l)
      | x >= l = throwing _TupleOutOfBounds (x, l)
      | otherwise = return $ ts !! i
  in
    f i
