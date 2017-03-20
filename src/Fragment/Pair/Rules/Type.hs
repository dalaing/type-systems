{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Pair.Rules.Type (
    PairNormalizeConstraint
  , pairNormalizeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.Pair.Ast.Type

type PairNormalizeConstraint ki ty a = AsTyPair ki ty

normalizePair :: PairNormalizeConstraint ki ty a
              => (Type ki ty a -> Type ki ty a)
              -> Type ki ty a
              -> Maybe (Type ki ty a)
normalizePair normalizeFn ty = do
  (p1, p2) <- preview _TyPair ty
  return $ review _TyPair (normalizeFn p1, normalizeFn p2)

pairNormalizeRules :: PairNormalizeConstraint ki ty a
                   => NormalizeInput ki ty a
pairNormalizeRules =
  NormalizeInput [ NormalizeTypeRecurse normalizePair ]
