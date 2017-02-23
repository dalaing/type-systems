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
module Fragment.STLC.Ast.Error (
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

import Fragment.STLC.Ast.Type

data ErrExpectedTyArr ty a = ErrExpectedTyArr (Type ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyArr

class AsExpectedTyArr e ty a | e -> ty, e -> a where
  _ExpectedTyArr :: Prism' e (Type ty a)

instance AsExpectedTyArr (ErrExpectedTyArr ty a) ty a where
  _ExpectedTyArr = _ErrExpectedTyArr

instance {-# OVERLAPPABLE #-} AsExpectedTyArr ((ErrSum xs) ty pt tm a) ty a => AsExpectedTyArr (ErrSum (x ': xs) ty pt tm a) ty a where
  _ExpectedTyArr = _ErrNext . _ExpectedTyArr

instance {-# OVERLAPPING #-} AsExpectedTyArr (ErrSum (ErrExpectedTyArr ty a ': xs) ty pt tm a) ty a where
  _ExpectedTyArr = _ErrNow . _ExpectedTyArr

expectTyArr :: (MonadError e m, AsExpectedTyArr e ty a, AsTySTLC ty) => Type ty a -> m (Type ty a, Type ty a)
expectTyArr ty =
  case preview _TyArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedTyArr ty
