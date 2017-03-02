{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Rules.Type (
    NormalizeTypeRule(..)
  , TypeInput(..)
  , TypeOutput(..)
  , prepareType
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Ast.Type

data NormalizeTypeRule ki ty =
    NormalizeTypeBase (forall a. Type ki ty a -> Maybe (Type ki ty a))
  | NormalizeTypeRecurse (forall a. (forall b. Type ki ty b -> Type ki ty b) -> Type ki ty a -> Maybe (Type ki ty a))

fixNormalizeTypeRule :: (forall b. Type ki ty b -> Type ki ty b)
                     -> NormalizeTypeRule ki ty
                     -> Type ki ty a
                     -> Maybe (Type ki ty a)
fixNormalizeTypeRule _ (NormalizeTypeBase f) = f
fixNormalizeTypeRule normFn (NormalizeTypeRecurse f) = f normFn

mkNormalizeType :: [NormalizeTypeRule ki ty]
                -> (forall a. Type ki ty a -> Type ki ty a)
mkNormalizeType rules =
  let
    go ty =
      fromMaybe ty .
      asum .
      fmap (\r -> fixNormalizeTypeRule (mkNormalizeType rules) r ty) $
      rules
  in
    go

data TypeInput ki ty a =
  TypeInput {
    tiNormalizeTypeRules :: [NormalizeTypeRule ki ty]
  }

instance Monoid (TypeInput ki ty a) where
  mempty =
    TypeInput
      mempty
  mappend (TypeInput n1) (TypeInput n2) =
    TypeInput
      (mappend n1 n2)

data TypeOutput ki ty a =
  TypeOutput {
    toNormalizeType :: Type ki ty a -> Type ki ty a
  }

prepareType :: TypeInput ki ty a -> TypeOutput ki ty a
prepareType (TypeInput nt) =
  TypeOutput (mkNormalizeType nt)
