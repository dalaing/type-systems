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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Fragment.Variant (
    TyFVariant(..)
  , AsTyVariant(..)
  , PtFVariant(..)
  , AsPtVariant(..)
  , TmFVariant(..)
  , AsTmVariant(..)
  , AsExpectedTyVariant(..)
  , AsVariantNotFound(..)
  , VariantContext
  , variantFragmentLazy
  , variantFragmentStrict
  , tyVariant
  , ptVariant
  , tmVariant
  ) where

-- TODO this should be split into lazy and strict versions
-- - the strict version should make sure the term in the case is a value before proceeding
-- - the lazy version will just match the tag and substitute the unevaluated term

import Control.Monad (MonadPlus(..))
import Text.Show

import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Fragment.Ast
import Fragment.Var
import Util
import Error

data TyFVariant f a =
  TyVariantF (N.NonEmpty (T.Text, f a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFVariant

deriveEq1 ''NonEmpty
deriveOrd1 ''NonEmpty
deriveShow1 ''NonEmpty

deriveEq1 ''TyFVariant
deriveOrd1 ''TyFVariant
deriveShow1 ''TyFVariant

instance EqRec TyFVariant where
  liftEqRec eR _ (TyVariantF vs1) (TyVariantF vs2) =
    let
      f (l1, v1) (l2, v2) = l1 == l2 && eR v1 v2
    in
      and $ N.zipWith f vs1 vs2

instance OrdRec TyFVariant where
  liftCompareRec cR _ (TyVariantF vs1) (TyVariantF vs2) =
    let
      f [] [] = EQ
      f [] (_ : _) = LT
      f (_ : _) [] = GT
      f ((lx, x): xs) ((ly, y): ys) =
        case compare lx ly of
          EQ -> case cR x y of
            EQ -> f xs ys
            z -> z
          z -> z
    in
      f (N.toList vs1) (N.toList vs2)

instance ShowRec TyFVariant where
  liftShowsPrecRec sR _ _ _ n (TyVariantF xs) =
    let
      g m (l, x) = showString ("(" ++ T.unpack l ++ ", ") .
                 sR m x .
                 showString ")"
      f _ ps = showListWith (g 0) ps
    in
      showsUnaryWith f "TyVariantF" n (N.toList xs)

instance Bound TyFVariant where
  TyVariantF tys >>>= f = TyVariantF (fmap (fmap (>>= f)) tys)

instance Bitransversable TyFVariant where
  bitransverse fT fL (TyVariantF ps) = TyVariantF <$> traverse (traverse (fT fL)) ps

class AsTyVariant ty where
  _TyVariantP :: Prism' (ty k a) (TyFVariant k a)

  _TyVariant :: Prism' (Type ty a) (N.NonEmpty (T.Text, Type ty a))
  _TyVariant = _TyTree . _TyVariantP . _TyVariantF

instance AsTyVariant TyFVariant where
  _TyVariantP = id

data PtFVariant f a =
  PtVariantF T.Text (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFVariant

deriveEq1 ''PtFVariant
deriveOrd1 ''PtFVariant
deriveShow1 ''PtFVariant

instance Bound PtFVariant where
  PtVariantF l p >>>= f = PtVariantF l (p >>= f)

instance Bitransversable PtFVariant where
  bitransverse fT fL (PtVariantF l pt) = PtVariantF l <$> fT fL pt

class AsPtVariant pt where
  _PtVariantP :: Prism' (pt k a) (PtFVariant k a)

  _PtVariant :: Prism' (Pattern pt a) (T.Text, Pattern pt a)
  _PtVariant = _PtTree . _PtVariantP . _PtVariantF

instance AsPtVariant PtFVariant where
  _PtVariantP = id

data TmFVariant (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmVariantF T.Text (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFVariant

instance (EqRec ty, Eq1 tm, Monad tm) => Eq1 (TmFVariant ty pt tm) where
  liftEq = $(makeLiftEq ''TmFVariant)

instance (OrdRec ty, Ord1 tm, Monad tm) => Ord1 (TmFVariant ty pt tm) where
  liftCompare = $(makeLiftCompare ''TmFVariant)

instance (ShowRec ty, Show1 tm) => Show1 (TmFVariant ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFVariant)

instance Bound (TmFVariant ty pt) where
  TmVariantF t tm ty >>>= f = TmVariantF t (tm >>= f) (ty >>= f)

instance Bitransversable (TmFVariant ty pt) where
  bitransverse fT fL (TmVariantF l tm ty) = TmVariantF <$> pure l <*> fT fL tm <*> fT fL ty

class AstTransversable ty pt tm => AsTmVariant ty pt tm where
  _TmVariantP :: Prism' (tm ty pt k a) (TmFVariant ty pt k a)

  _TmVariant :: Prism' (Term ty pt tm a) (T.Text, Term ty pt tm a, Type ty a)
  _TmVariant = _Wrapped . _ATerm . _TmVariantP . _TmVariantF . mkTriple id _Unwrapped _Type

instance (Bitransversable ty, Bitransversable pt) => AsTmVariant ty pt TmFVariant where
  _TmVariantP = id

-- Errors

class AsExpectedTyVariant e ty a | e -> ty, e -> a where
  _ExpectedTyVariant :: Prism' e (Type ty a)

expectTyVariant :: (MonadError e m, AsExpectedTyVariant e ty a, AsTyVariant ty) => Type ty a -> m (N.NonEmpty (T.Text, Type ty a))
expectTyVariant ty =
  case preview _TyVariant ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyVariant ty

class AsVariantNotFound e where
  _VariantNotFound :: Prism' e T.Text

lookupVariant :: (MonadError e m, AsVariantNotFound e) =>  N.NonEmpty (T.Text, t a) -> T.Text -> m (t a)
lookupVariant ts t =
  case lookup t (N.toList ts) of
    Just x -> return x
    Nothing -> throwing _VariantNotFound t

-- Rules

valueVariant :: (AsTmVariant ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
valueVariant valueFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  tm' <- valueFn tmV
  return $ review _TmVariant (l, tm', ty)

stepVariant :: (AsTmVariant ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepVariant stepFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  tm' <- stepFn tmV
  return $ review _TmVariant (l, tm', ty)

inferTmVariant :: (Eq a, EqRec ty, MonadError e m, AsExpectedTyVariant e ty a, AsVariantNotFound e, AsExpectedEq e ty a, AsTyVariant ty, AsTmVariant ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmVariant inferFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  return $ do
    tyL <- inferFn tmV
    tys <- expectTyVariant ty
    tyV <- lookupVariant tys l
    expectEq tyL tyV
    return ty

matchVariant :: (AsPtVariant pt, AsTmVariant ty pt tm) => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
matchVariant matchFn p tm = do
  (lP, pV) <- preview _PtVariant p
  (lV, tmV, _) <- preview _TmVariant tm
  if lP == lV
  then matchFn pV tmV
  else mzero

checkVariant :: (MonadError e m, AsExpectedTyVariant e ty a, AsVariantNotFound e, AsPtVariant pt, AsTyVariant ty) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkVariant checkFn p ty = do
  (lV, pV) <- preview _PtVariant p
  return $ do
    vs <- expectTyVariant ty
    tyV <- lookupVariant vs lV
    checkFn pV tyV

type VariantContext e s r m ty pt tm a = (Ord a, EqRec ty, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsExpectedTyVariant e ty a, AsExpectedAllEq e ty a, AsVariantNotFound e, AsExpectedEq e ty a, AsTyVariant ty, AsPtVariant pt, AsTmVariant ty pt tm)

evalRulesStrict :: VariantContext e s r m ty p tm a => FragmentInput e s r m ty p tm a
evalRulesStrict =
  FragmentInput
    [ ValueRecurse valueVariant ]
    [ EvalStep stepVariant ]
    [] [] [] [] []

baseRules :: VariantContext e s r m ty p tm a => FragmentInput e s r m ty p tm a
baseRules =
  FragmentInput [] []
    [ InferRecurse inferTmVariant ]
    [ PMatchRecurse matchVariant ]
    [ PCheckRecurse checkVariant ]
    [] []

variantFragmentStrict :: VariantContext e s r m ty p tm a => FragmentInput e s r m ty p tm a
variantFragmentStrict = evalRulesStrict `mappend` baseRules

variantFragmentLazy :: VariantContext e s r m ty p tm a => FragmentInput e s r m ty p tm a
variantFragmentLazy = baseRules

-- Helpers

tyVariant :: AsTyVariant ty => N.NonEmpty (T.Text, Type ty a) -> Type ty a
tyVariant = review _TyVariant

ptVariant :: AsPtVariant pt => T.Text -> Pattern pt a -> Pattern pt a
ptVariant = curry $ review _PtVariant

tmVariant :: AsTmVariant ty pt tm => T.Text -> Term ty pt tm a -> Type ty a -> Term ty pt tm a
tmVariant l tm ty = review _TmVariant (l, tm, ty)
