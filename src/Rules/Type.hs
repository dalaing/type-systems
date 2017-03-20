{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rules.Type (
    NormalizeTypeRule(..)
  , NormalizeInput(..)
  , NormalizeOutput(..)
  , BasicNormalizeConstraint
  , NormalizeRules(..)
  , normalizeOutput
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))

import GHC.Exts (Constraint)

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

data NormalizeInput ki ty a =
  NormalizeInput {
    niNormalizeTypeRules :: [NormalizeTypeRule ki ty]
  }

instance Monoid (NormalizeInput ki ty a) where
  mempty =
    NormalizeInput
      mempty
  mappend (NormalizeInput n1) (NormalizeInput n2) =
    NormalizeInput
      (mappend n1 n2)

data NormalizeOutput ki ty a =
  NormalizeOutput {
    noNormalizeType :: Type ki ty a -> Type ki ty a
  }

prepareNormalize :: NormalizeInput ki ty a -> NormalizeOutput ki ty a
prepareNormalize (NormalizeInput nt) =
  NormalizeOutput (mkNormalizeType nt)

-- We use this to keep the kinds happy elsewhere
type BasicNormalizeConstraint (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a = (() :: Constraint)

class NormalizeRules (k :: j) where
  type NormalizeConstraint (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a k :: Constraint

  normalizeInput :: NormalizeConstraint ki ty a k
                 => Proxy k
                 -> NormalizeInput ki ty a

instance NormalizeRules '[] where
  type NormalizeConstraint ki ty a '[] =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance (NormalizeRules k, NormalizeRules ks) => NormalizeRules (k ': ks) where
  type NormalizeConstraint ki ty a (k ': ks) =
    ( NormalizeConstraint ki ty a k
    , NormalizeConstraint ki ty a ks
    )

  normalizeInput _ =
    mappend
      (normalizeInput (Proxy :: Proxy k))
      (normalizeInput (Proxy :: Proxy ks))

normalizeOutput :: (NormalizeRules k, NormalizeConstraint ki ty a k)
                => Proxy k
                -> NormalizeOutput ki ty a
normalizeOutput k =
  prepareNormalize (normalizeInput k)
