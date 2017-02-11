{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Typed.SystemF.Type (
    Type
  , tyVar
  , tyAll
  , tyArr
  , tyInt
  ) where

import Control.Lens

import Bound

import Typed.SystemF.Internal
import Typed.SystemF.Type.Class

newtype Type a = Type (AST (ASTVar a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeWrapped ''Type

instance AsVar (Type a) a where
  _Var = _Wrapped . _AVar . _ASTTyVar

instance AsTypeF (Type a) AST (ASTVar a) where
  _TypeF = _Wrapped . _TypeF

instance AsType Type where
  _TyVar = _Var
  _TyAll = prism (uncurry toTyAll) (uncurry fromTyAll)
  -- _TyArr = prism (uncurry tyArr) (fromTyArr)
  _TyArr = _TyFArr . bimapping _Unwrapped _Unwrapped
  _TyInt = _TyFInt

  substTyAll = substAll

tyVar :: a -> Type a
tyVar = review _Var

substAll :: Type a -> Type a -> Maybe (Type a)
substAll ty tyA = do
  ty' <- preview _Wrapped ty
  tyA' <- preview _TyFAll tyA
  return . review _Wrapped $ instantiate1 ty' tyA'

tyAll :: Eq a => a -> Type a -> Type a
tyAll v = snd . toTyAll v

toTyAll :: Eq a => a -> Type a -> (a, Type a)
toTyAll v (Type ty) = (v, review _TyFAll (abstract1 (review _ASTTyVar v) ty))

fromTyAll :: a -> Type a -> Either (a, Type a) (a, Type a)
fromTyAll v ty =
  case preview _TyFAll ty of
    Just s -> Right (v, review _Wrapped (instantiate1 (review (_AVar . _ASTTyVar) v) s))
    Nothing -> Left (v, ty)

tyArr :: Type a
      -> Type a
      -> Type a
tyArr x y = review _TyArr (x, y)

tyInt :: Type a
tyInt = review _TyInt ()
