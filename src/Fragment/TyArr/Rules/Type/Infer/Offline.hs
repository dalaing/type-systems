{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.TyArr.Rules.Type.Infer.Offline (
    TyArrInferTypeContext
  , tyArrInferTypeRules
  ) where

import Control.Lens (preview)
import Data.Equivalence.Monad (EquivT, classDesc)

import Ast.Type
import Rules.Unification
import Rules.Type.Infer.Offline

import Fragment.TyArr.Ast.Type

unifyArr :: (UnificationContext e m (Type ki ty) a, AsTyArr ki ty)
          => ([Type ki ty a] -> [Type ki ty a] -> EquivT s (Type ki ty a) (Type ki ty a) m ())
          -> UConstraint (Type ki ty) a
          -> Maybe (EquivT s (Type ki ty a) (Type ki ty a) m ())
unifyArr unifyMany (UCEq ty1 ty2) = do
  (p1a, p1b) <- preview _TyArr ty1
  (p2a, p2b) <- preview _TyArr ty2
  return $ do
    c1a <- classDesc p1a
    c1b <- classDesc p1b
    c2a <- classDesc p2a
    c2b <- classDesc p2b
    unifyMany [c1a, c1b] [c2a, c2b]

type TyArrInferTypeContext e w s r m ki ty (pt :: (* -> *) -> * -> *) (tm :: (* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = (UnificationContext e m (Type ki ty) a, AsTyArr ki ty)

tyArrInferTypeRules :: TyArrInferTypeContext e w s r m ki ty pt tm a
                    => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
tyArrInferTypeRules =
  InferTypeInput
    [UnificationMany unifyArr]
    []
    []
