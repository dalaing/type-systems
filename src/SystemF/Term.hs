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
module SystemF.Term (
    Term
  , tmVar
  , tmLam
  , tmApp
  , tmLamTy
  , tmAppTy
  , tmInt
  , tmAdd
  ) where

import Control.Lens

import Bound

import SystemF.Internal
import SystemF.Term.Class
import SystemF.Type

newtype Term (ty :: * -> *) a = Term (AST (ASTVar a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeWrapped ''Term

instance AsVar (Term ty a) a where
  _Var = _Wrapped . _AVar . _ASTTmVar

instance AsTermF (Term ty a) AST (ASTVar a) where
  _TermF = _Wrapped . _TermF

instance AsTerm Type (Term Type) where
  _TmVar = _Var
  _TmLam = prism (\(x,y,z) -> toTmLam x y z) (uncurry fromTmLam)
  _TmApp = _TmFApp . bimapping _Unwrapped _Unwrapped
  _TmLamTy = prism (uncurry toTmLamTy) (uncurry fromTmLamTy)
  _TmAppTy = _TmFAppTy . bimapping _Unwrapped _Unwrapped
  _TmAdd = _TmFAdd . bimapping _Unwrapped _Unwrapped
  _TmInt = _TmFInt

  reduceLamApp = lamApp
  reduceLamTyAppTy = lamTyAppTy

lamApp :: Term Type a -> Maybe (Term Type a)
lamApp tm = do
  (f, x) <- preview _TmFApp tm
  (_, s) <- preview _TmFLam f
  return . review _Wrapped $ instantiate1 x s

lamTyAppTy :: Term Type a -> Maybe (Term Type a)
lamTyAppTy tm = do
  (f, x) <- preview _TmFAppTy tm
  s <- preview _TmFLamTy f
  return . review _Wrapped $ instantiate1 x s

tmVar :: a -> Term Type a
tmVar = review _Var

tmLam :: Eq a => a -> Type a -> Term Type a -> Term Type a
tmLam x ty = snd . toTmLam x ty

toTmLam :: Eq a => a -> Type a -> Term Type a -> (a, Term Type a)
toTmLam v ty tm =
  let
    aTy = view _Wrapped ty
    aTm = view _Wrapped tm
    aV = review _ASTTmVar v
    tm' = review _TmFLam (aTy, abstract1 aV aTm)
  in
    (v, tm')

fromTmLam :: a -> Term Type a -> Either (a, Term Type a) (a, Type a, Term Type a)
fromTmLam x tm = do
  case preview _TmFLam tm of
    Just (ty, s) ->
      let
        ty' = review _Wrapped ty
        x' = review (_AVar . _ASTTmVar) x
        tm' = review _Wrapped $ instantiate1 x' s
      in
        Right (x, ty', tm')
    Nothing -> Left (x, tm)

tmApp :: Term Type a -> Term Type a -> Term Type a
tmApp x y = review _TmApp (x, y)

tmLamTy :: Eq a => a -> Term Type a -> Term Type a
tmLamTy v = snd . toTmLamTy v

toTmLamTy :: Eq a => a -> Term Type a -> (a, Term Type a)
toTmLamTy v tm =
  let
    aTm = view _Wrapped tm
    aVar = review _ASTTyVar v
    tm' = review _TmFLamTy (abstract1 aVar aTm)
  in
    (v, tm')

fromTmLamTy :: a -> Term Type a -> Either (a, Term Type a) (a, Term Type a)
fromTmLamTy x tm = do
  case preview _TmFLamTy tm of
    Just s ->
      let
        x' = review (_AVar . _ASTTyVar) x
        tm' = review _Wrapped $ instantiate1 x' s
      in
        Right (x, tm')
    Nothing -> Left (x, tm)

tmAppTy :: Term Type a -> Type a -> Term Type a
tmAppTy x y = review _TmAppTy (x, y)

tmAdd :: Term Type a -> Term Type a -> Term Type a
tmAdd x y = review _TmAdd (x, y)

tmInt :: Int -> Term Type a
tmInt = review _TmInt
