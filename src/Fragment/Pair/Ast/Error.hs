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
module Fragment.Pair.Ast.Error (
    ErrExpectedTyPair(..)
  , AsExpectedTyPair(..)
  , expectTyPair
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens (preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Type
import Ast.Error

import Fragment.Pair.Ast.Type

data ErrExpectedTyPair ki ty a = ErrExpectedTyPair (Type ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyPair

class AsExpectedTyPair e ki ty a where -- | e -> ty, e -> a where
  _ExpectedTyPair :: Prism' e (Type ki ty a)

instance AsExpectedTyPair (ErrExpectedTyPair ki ty a) ki ty a where
  _ExpectedTyPair = _ErrExpectedTyPair

instance {-# OVERLAPPABLE #-} AsExpectedTyPair (ErrSum xs) ki ty a => AsExpectedTyPair (ErrSum (x ': xs)) ki ty a where
  _ExpectedTyPair = _ErrNext . _ExpectedTyPair

instance {-# OVERLAPPING #-} AsExpectedTyPair (ErrSum (ErrExpectedTyPair ki ty a ': xs)) ki ty a where
  _ExpectedTyPair = _ErrNow . _ExpectedTyPair

expectTyPair :: (MonadError e m, AsExpectedTyPair e ki ty a, AsTyPair ki ty) => Type ki ty a -> m (Type ki ty a, Type ki ty a)
expectTyPair ty =
  case preview _TyPair ty of
    Just (ty1, ty2) -> return (ty1, ty2)
    _ -> throwing _ExpectedTyPair ty
