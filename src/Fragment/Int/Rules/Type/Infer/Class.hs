{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Fragment.Int.Rules.Type.Infer.Class (
    IntInferTypeHelper(..)
  ) where

import Data.Proxy (Proxy)
import GHC.Exts (Constraint)

import Ast.Type
import Rules.Type.Infer.Common

class MkInferType i => IntInferTypeHelper i t where
  type IntInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a i t :: Constraint

  createInt :: IntInferTypeHelperConstraint e w s r m ki ty a i t
            => Proxy (MonadProxy e w s r m)
            -> Proxy i
            -> Proxy t
            -> InferTypeMonad ki ty a m i (Type ki ty a)
