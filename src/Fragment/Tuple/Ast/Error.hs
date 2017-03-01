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

data ErrExpectedTyTuple ki ty a = ErrExpectedTyTuple (Type ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyTuple

class AsExpectedTyTuple e ki ty a where -- | e -> ty, e -> a where
  _ExpectedTyTuple :: Prism' e (Type ki ty a)

instance AsExpectedTyTuple (ErrExpectedTyTuple ki ty a) ki ty a where
  _ExpectedTyTuple = _ErrExpectedTyTuple

instance {-# OVERLAPPABLE #-} AsExpectedTyTuple (ErrSum xs) ki ty a => AsExpectedTyTuple (ErrSum (x ': xs)) ki ty a where
  _ExpectedTyTuple = _ErrNext . _ExpectedTyTuple

instance {-# OVERLAPPING #-} AsExpectedTyTuple (ErrSum (ErrExpectedTyTuple ki ty a ': xs)) ki ty a where
  _ExpectedTyTuple = _ErrNow . _ExpectedTyTuple

expectTyTuple :: (MonadError e m, AsExpectedTyTuple e ki ty a, AsTyTuple ki ty) => Type ki ty a -> m [Type ki ty a]
expectTyTuple ty =
  case preview _TyTuple ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyTuple ty

data ErrTupleOutOfBounds = ErrTupleOutOfBounds Int Int
  deriving (Eq, Ord, Show)

makePrisms ''ErrTupleOutOfBounds

class AsTupleOutOfBounds e where
  _TupleOutOfBounds :: Prism' e (Int, Int)

instance AsTupleOutOfBounds ErrTupleOutOfBounds where
  _TupleOutOfBounds = _ErrTupleOutOfBounds

instance {-# OVERLAPPABLE #-} AsTupleOutOfBounds (ErrSum xs) => AsTupleOutOfBounds (ErrSum (x ': xs)) where
  _TupleOutOfBounds = _ErrNext . _TupleOutOfBounds

instance {-# OVERLAPPING #-} AsTupleOutOfBounds (ErrSum (ErrTupleOutOfBounds ': xs)) where
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
