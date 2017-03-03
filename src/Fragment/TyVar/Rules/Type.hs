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
    TyVarTypeContext
  , tyVarTypeRules
  ) where

import GHC.Exts (Constraint)

import Control.Lens (preview)

import Rules.Type
import Ast.Type

type TyVarTypeContext (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a = (() :: Constraint)

normalizeTyVar :: TyVarTypeContext ki ty a
               => Type ki ty a
               -> Maybe (Type ki ty a)
normalizeTyVar ty = do
  _ <- preview _TyVar ty
  return ty

tyVarTypeRules :: TyVarTypeContext ki ty a
              => TypeInput ki ty a
tyVarTypeRules =
  TypeInput [ NormalizeTypeBase normalizeTyVar ]
