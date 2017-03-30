{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Fragment.IsoRec.Rules.Type (
    IsoRecNormalizeConstraint
  , isoRecNormalizeRules
  ) where


import Bound (Bound)
import Control.Lens (review, preview)

import Ast.Type
import Data.Bitransversable
import Rules.Type

import Fragment.IsoRec.Ast.Type

type IsoRecNormalizeConstraint ki ty a =
  ( AsTyIsoRec ki ty
  , Bound ki
  , Bitransversable ki
  )

normalizeRec :: IsoRecNormalizeConstraint ki ty a
             => (forall b. Type ki ty b -> Type ki ty b)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeRec normalizeFn ty = do
  s <- preview _TyRec ty
  return $ review _TyRec (scopeAppTy normalizeFn s)

isoRecNormalizeRules :: IsoRecNormalizeConstraint ki ty a
                     => NormalizeInput ki ty a
isoRecNormalizeRules =
  NormalizeInput
    [ NormalizeTypeRecurse normalizeRec ]
