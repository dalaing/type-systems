{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module Fragment.TyVar.Rules.Type (
    tyVarNormalizeRules
  ) where

import Control.Lens (preview)

import Rules.Type
import Ast.Type

normalizeTyVar :: Type ki ty a
               -> Maybe (Type ki ty a)
normalizeTyVar ty = do
  _ <- preview _TyVar ty
  return ty

tyVarNormalizeRules :: NormalizeInput ki ty a
tyVarNormalizeRules =
  NormalizeInput [ NormalizeTypeBase normalizeTyVar ]
