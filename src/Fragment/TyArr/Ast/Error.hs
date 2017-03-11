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
module Fragment.TyArr.Ast.Error (
    ErrExpectedTyArr(..)
  , AsExpectedTyArr(..)
  , expectTyArr
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens (preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Type
import Ast.Error

import Fragment.TyArr.Ast.Type

data ErrExpectedTyArr ki ty a = ErrExpectedTyArr (Type ki ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyArr

class AsExpectedTyArr e ki ty a where -- | e -> ty, e -> a where
  _ExpectedTyArr :: Prism' e (Type ki ty a)

instance AsExpectedTyArr (ErrExpectedTyArr ki ty a) ki ty a where
  _ExpectedTyArr = _ErrExpectedTyArr

instance {-# OVERLAPPABLE #-} AsExpectedTyArr (ErrSum xs) ki ty a => AsExpectedTyArr (ErrSum (x ': xs)) ki ty a where
  _ExpectedTyArr = _ErrNext . _ExpectedTyArr

instance {-# OVERLAPPING #-} AsExpectedTyArr (ErrSum (ErrExpectedTyArr ki ty a ': xs)) ki ty a where
  _ExpectedTyArr = _ErrNow . _ExpectedTyArr

expectTyArr :: (MonadError e m, AsExpectedTyArr e ki ty a, AsTyArr ki ty) => Type ki ty a -> m (Type ki ty a, Type ki ty a)
expectTyArr ty =
  case preview _TyArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedTyArr ty
