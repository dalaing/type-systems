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
module Context.Term.Error (
    ErrUnboundTermVariable(..)
  , AsUnboundTermVariable(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Error

data ErrUnboundTermVariable a = ErrUnboundTermVariable a
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnboundTermVariable

class AsUnboundTermVariable e a | e -> a where
  _UnboundTermVariable :: Prism' e a

instance AsUnboundTermVariable (ErrUnboundTermVariable a) a where
  _UnboundTermVariable = _ErrUnboundTermVariable

instance {-# OVERLAPPABLE #-} AsUnboundTermVariable ((ErrSum xs) ty pt tm a) a => AsUnboundTermVariable (ErrSum (x ': xs) ty pt tm a) a where
  _UnboundTermVariable = _ErrNext . _UnboundTermVariable

instance {-# OVERLAPPING #-} AsUnboundTermVariable (ErrSum (ErrUnboundTermVariable a ': xs) ty pt tm a) a where
  _UnboundTermVariable = _ErrNow . _UnboundTermVariable
