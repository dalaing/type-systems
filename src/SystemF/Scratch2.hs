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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
module SystemF.Scratch2 (
    Type
  , AsType(..)
  , tyVar
  , tyArr
  , tyAll
  , tyInt
  , Term
  , AsTerm(..)
  , tmVar
  , tmLam
  , tmApp
  , tmLamTy
  , tmAppTy
  , tmInt
  , tmAdd
  ) where

import Control.Monad (ap)

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

-- abstract :: Monad f => (a -> Maybe b) -> f a -> Scope b f a
-- instantiate :: Monad f => (b -> f a) -> Scope b f a -> f a

-- TODO possible Either Void () and Either () Void
-- as the b in Scope
data Blob a =
    BVar a
  | BTmLam (Blob a) (Scope () Blob a)
  | BTmApp (Blob a) (Blob a)
  | BTmLamTy (Scope () Blob a)
  | BTmAppTy (Blob a) (Blob a)
  | BTyAll (Scope () Blob a)
  | BTyArr (Blob a) (Blob a)
  | BTyInt
  | BTmInt Int
  | BTmAdd (Blob a) (Blob a)
  deriving (Functor, Foldable, Traversable)

makePrisms ''Blob

deriveEq1   ''Blob
deriveOrd1  ''Blob
deriveShow1 ''Blob

instance (Eq a) => Eq (Blob a) where (==) = eq1
instance (Ord a) => Ord (Blob a) where compare = compare1
instance (Show a) => Show (Blob a) where showsPrec = showsPrec1

instance Applicative Blob where
  pure = return
  (<*>) = ap

instance Monad Blob where
  return = BVar

  BVar x >>= f = f x
  BTmLam t s >>= f = BTmLam (t >>= f) (s >>>= f)
  BTmApp g x >>= f = BTmApp (g >>= f) (x >>= f)
  BTmLamTy s >>= f = BTmLamTy (s >>>= f)
  BTmAppTy g x >>= f = BTmAppTy (g >>= f) (x >>= f)
  BTyAll s >>= f = BTyAll (s >>>= f)
  BTyArr g x >>= f = BTyArr (g >>= f) (x >>= f)
  BTyInt >>= _ = BTyInt
  BTmInt i >>= _ = BTmInt i
  BTmAdd x y >>= f = BTmAdd (x >>= f) (y >>= f)

data SVar a =
    VTerm a
  | VType a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''SVar

newtype Type a = Type { unType :: Blob (SVar a) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeWrapped ''Type

class AsType ty where
  _TyVar :: Prism' (ty a) a
  _TyAll :: Eq a => Prism' (a, ty a) (a, ty a)
  _TyArr :: Prism' (ty a) (ty a, ty a)
  _TyInt :: Prism' (ty a) ()

  substTyAll :: ty a -> ty a -> Maybe (ty a)

instance AsType Type where
  _TyVar = prism tyVar fromTyVar
  _TyAll = prism (uncurry toTyAll) (uncurry fromTyAll)
  _TyArr = prism (uncurry tyArr) fromTyArr
  _TyInt = prism (const tyInt) fromTyInt

  substTyAll = substAll

substAll :: Type a -> Type a -> Maybe (Type a)
substAll (Type ty) (Type (BTyAll s)) = Just . Type $ instantiate1 ty s
substAll _ _ = Nothing

tyVar :: a -> Type a
tyVar = Type . BVar . VType

fromTyVar :: Type a -> Either (Type a) a
fromTyVar (Type (BVar (VType x))) =
  Right x
fromTyVar ty =
  Left ty

tyAll :: Eq a => a -> Type a -> Type a
tyAll v = snd . toTyAll v

toTyAll :: Eq a => a -> Type a -> (a, Type a)
toTyAll v (Type ty) = (v, Type (BTyAll (abstract1 (VType v) ty)))

fromTyAll :: a -> Type a -> Either (a, Type a) (a, Type a)
fromTyAll x (Type (BTyAll s)) = Right (x, Type (instantiate1 (BVar (VType x)) s))
fromTyAll x ty = Left (x, ty)

tyArr :: Type a -> Type a -> Type a
tyArr (Type x) (Type y) = Type (BTyArr x y)

fromTyArr :: Type a -> Either (Type a) (Type a, Type a)
fromTyArr (Type (BTyArr ty1 ty2)) =
  Right (Type ty1, Type ty2)
fromTyArr ty =
  Left ty

tyInt :: Type a
tyInt = Type BTyInt

fromTyInt :: Type a -> Either (Type a) ()
fromTyInt (Type BTyInt) = Right ()
fromTyInt ty = Left ty

newtype Term ty a = Term { unTerm :: Blob (SVar a) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeWrapped ''Term

class AsTerm ty tm | tm -> ty where
  _TmVar   :: Prism' (tm a) a
  _TmLam   :: Eq a => Prism' (a, tm a) (a, ty a, tm a)
  _TmApp   :: Prism' (tm a) (tm a, tm a)
  _TmLamTy :: Eq a => Prism' (a, tm a) (a, tm a)
  _TmAppTy :: Prism' (tm a) (tm a, ty a)
  _TmInt   :: Prism' (tm a) Int
  _TmAdd   :: Prism' (tm a) (tm a, tm a)

  reduceLamApp :: tm a -> Maybe (tm a)
  reduceLamTyAppTy :: tm a -> Maybe (tm a)

instance AsTerm Type (Term Type) where
  _TmVar = prism tmVar fromTmVar
  _TmLam = prism (\(x,y,z) -> toTmLam x y z) (uncurry fromTmLam)
  _TmApp = prism (uncurry tmApp) fromTmApp
  _TmLamTy = prism (uncurry toTmLamTy) (uncurry fromTmLamTy)
  _TmAppTy = prism (uncurry tmAppTy) fromTmAppTy
  _TmAdd = prism (uncurry tmAdd) fromTmAdd
  _TmInt = prism tmInt fromTmInt
  reduceLamApp = lamApp
  reduceLamTyAppTy = lamTyAppTy

lamApp :: Term Type a -> Maybe (Term Type a)
lamApp (Term (BTmApp (BTmLam _ s) x)) = Just . Term $ instantiate1 x s
lamApp _ = Nothing

lamTyAppTy :: Term Type a -> Maybe (Term Type a)
lamTyAppTy (Term (BTmAppTy (BTmLamTy s) x)) = Just . Term $ instantiate1 x s
lamTyAppTy _ = Nothing

tmVar :: a -> Term Type a
tmVar = Term . BVar . VTerm

fromTmVar :: Term Type a -> Either (Term Type a) a
fromTmVar (Term (BVar (VTerm x))) = Right x
fromTmVar tm = Left tm

tmLam :: Eq a => a -> Type a -> Term Type a -> Term Type a
tmLam x ty = snd . toTmLam x ty

toTmLam :: Eq a => a -> Type a -> Term Type a -> (a, Term Type a)
toTmLam v (Type ty) (Term tm) = (v, Term (BTmLam ty (abstract1 (VTerm v) tm)))

fromTmLam :: a -> Term Type a -> Either (a, Term Type a) (a, Type a, Term Type a)
fromTmLam x (Term (BTmLam ty s)) = Right (x, Type ty, Term (instantiate1 (BVar (VTerm x)) s))
fromTmLam x tm = Left (x, tm)

tmApp :: Term Type a -> Term Type a -> Term Type a
tmApp (Term x) (Term y) = Term (BTmApp x y)

fromTmApp :: Term Type a -> Either (Term Type a) (Term Type a, Term Type a)
fromTmApp (Term (BTmApp tm1 tm2)) =
  Right (Term tm1, Term tm2)
fromTmApp tm =
  Left tm

tmLamTy :: Eq a => a -> Term Type a -> Term Type a
tmLamTy v = snd . toTmLamTy v

toTmLamTy :: Eq a => a -> Term Type a -> (a, Term Type a)
toTmLamTy v (Term tm) = (v, Term (BTmLamTy (abstract1 (VType v) tm)))

fromTmLamTy :: a -> Term Type a -> Either (a, Term Type a) (a, Term Type a)
fromTmLamTy x (Term (BTmLamTy s)) = Right (x, Term (instantiate1 (BVar (VType x)) s))
fromTmLamTy x tm = Left (x, tm)

tmAppTy :: Term Type a -> Type a -> Term Type a
tmAppTy (Term x) (Type y) = Term (BTmAppTy x y)

fromTmAppTy :: Term Type a -> Either (Term Type a) (Term Type a, Type a)
fromTmAppTy (Term (BTmAppTy tm ty)) =
  Right (Term tm, Type ty)
fromTmAppTy tm =
  Left tm

tmAdd :: Term Type a -> Term Type a -> Term Type a
tmAdd (Term x) (Term y) = Term (BTmAdd x y)

fromTmAdd :: Term Type a -> Either (Term Type a) (Term Type a, Term Type a)
fromTmAdd (Term (BTmAdd ty1 ty2)) =
  Right (Term ty1, Term ty2)
fromTmAdd ty =
  Left ty

tmInt :: Int -> Term Type a
tmInt i = Term (BTmInt i)

fromTmInt :: Term Type a -> Either (Term Type a) Int
fromTmInt (Term (BTmInt i)) = Right i
fromTmInt tm = Left tm
