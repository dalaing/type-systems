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
module Context.Type.Error (
    ErrUnboundTypeVariable(..)
  , AsUnboundTypeVariable(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Error

data ErrUnboundTypeVariable a = ErrUnboundTypeVariable a
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnboundTypeVariable

class AsUnboundTypeVariable e a where -- | e -> a where
  _UnboundTypeVariable :: Prism' e a

instance AsUnboundTypeVariable (ErrUnboundTypeVariable a) a where
  _UnboundTypeVariable = _ErrUnboundTypeVariable

instance {-# OVERLAPPABLE #-} AsUnboundTypeVariable (ErrSum xs) a => AsUnboundTypeVariable (ErrSum (x ': xs)) a where
  _UnboundTypeVariable = _ErrNext . _UnboundTypeVariable

instance {-# OVERLAPPING #-} AsUnboundTypeVariable (ErrSum (ErrUnboundTypeVariable a ': xs)) a where
  _UnboundTypeVariable = _ErrNow . _UnboundTypeVariable
