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
module Fragment.Bool (
    TyFBool(..)
  , AsTyBool(..)
  , PtFBool(..)
  , AsPtBool(..)
  , TmFBool(..)
  , AsTmBool(..)
  , BoolContext
  , boolFragment
  , tyBool
  , ptBool
  , tmBool
  , tmAnd
  , tmOr
  ) where

import Control.Monad (MonadPlus(..))

import Control.Monad.Except (MonadError)

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Fragment.Ast
import Error
import Util

data TyFBool (f :: * -> *) a =
  TyBoolF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFBool

deriveEq1 ''TyFBool
deriveOrd1 ''TyFBool
deriveShow1 ''TyFBool

instance EqRec TyFBool where
  liftEqRec _ _ TyBoolF TyBoolF = True

instance OrdRec TyFBool where
  liftCompareRec _ _ TyBoolF TyBoolF = EQ

instance ShowRec TyFBool where
  liftShowsPrecRec _ _ _ _ n TyBoolF = showsPrec n TyBoolF

instance Bound TyFBool where
  TyBoolF >>>= _ = TyBoolF

instance Bitransversable TyFBool where
  bitransverse _ _ TyBoolF = pure TyBoolF

class AsTyBool ty where
  _TyBoolP :: Prism' (ty k a) (TyFBool k a)

  _TyBool :: Prism' (Type ty a) ()
  _TyBool = _TyTree . _TyBoolP . _TyBoolF

instance AsTyBool TyFBool where
  _TyBoolP = id

data PtFBool (f :: * -> *) a =
  PtBoolF Bool
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFBool

deriveEq1 ''PtFBool
deriveOrd1 ''PtFBool
deriveShow1 ''PtFBool

instance EqRec PtFBool where
  liftEqRec _ _ (PtBoolF i) (PtBoolF j) = i == j

instance OrdRec PtFBool where
  liftCompareRec _ _ (PtBoolF i) (PtBoolF j) = compare i j

instance ShowRec PtFBool where
  liftShowsPrecRec _ _ _ _ = showsPrec

instance Bound PtFBool where
  PtBoolF b >>>= _ = PtBoolF b

instance Bitransversable PtFBool where
  bitransverse _ _ (PtBoolF b) = pure $ PtBoolF b

class AsPtBool pt where
  _PtBoolP :: Prism' (pt k a) (PtFBool k a)

  _PtBool :: Prism' (Pattern pt a) Bool
  _PtBool = _PtTree . _PtBoolP . _PtBoolF

instance AsPtBool PtFBool where
  _PtBoolP = id

data TmFBool (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmBoolF Bool
  | TmAndF (f a) (f a)
  | TmOrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFBool

deriveEq1 ''TmFBool
deriveOrd1 ''TmFBool
deriveShow1 ''TmFBool

instance EqRec (TmFBool ty pt) where
  liftEqRec _ _ (TmBoolF i) (TmBoolF j) = i == j
  liftEqRec eR _ (TmAndF x1 y1) (TmAndF x2 y2) = eR x1 x2 && eR y1 y2
  liftEqRec eR _ (TmOrF x1 y1) (TmOrF x2 y2) = eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFBool ty pt) where
  liftCompareRec _ _ (TmBoolF i) (TmBoolF j) = compare i j
  liftCompareRec _ _ (TmBoolF _) _ = LT
  liftCompareRec _ _ _ (TmBoolF _) = GT
  liftCompareRec cR _ (TmAndF x1 y1) (TmAndF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z
  liftCompareRec _ _ (TmAndF _ _) _ = LT
  liftCompareRec _ _ _ (TmAndF _ _) = GT
  liftCompareRec cR _ (TmOrF x1 y1) (TmOrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFBool ty pt) where
  liftShowsPrecRec _ _ _ _ n (TmBoolF i) =
    showsUnaryWith showsPrec "TmBoolF" n i
  liftShowsPrecRec sR _ _ _ n (TmAndF x y) =
    showsBinaryWith sR sR "TmAndF" n x y
  liftShowsPrecRec sR _ _ _ n (TmOrF x y) =
    showsBinaryWith sR sR "TmOrF" n x y

instance Bound (TmFBool ty pt) where
  TmBoolF b >>>= _ = TmBoolF b
  TmAndF x y >>>= f = TmAndF (x >>= f) (y >>= f)
  TmOrF x y >>>= f = TmOrF (x >>= f) (y >>= f)

instance Bitransversable (TmFBool ty pt) where
  bitransverse _ _ (TmBoolF b) = pure $ TmBoolF b
  bitransverse fT fL (TmAndF x y) = TmAndF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmOrF x y) = TmOrF <$> fT fL x <*> fT fL y

class AsTmBool ty pt tm where
  _TmBoolP :: Prism' (tm ty pt k a) (TmFBool ty pt k a)

  _TmBool :: Prism' (Term ty pt tm a) Bool
  _TmBool = _Wrapped . _ATerm . _TmBoolP . _TmBoolF

  _TmAnd :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmAnd = _Wrapped . _ATerm . _TmBoolP . _TmAndF . bimapping _Unwrapped _Unwrapped

  _TmOr :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmOr = _Wrapped . _ATerm . _TmBoolP . _TmOrF . bimapping _Unwrapped _Unwrapped

instance AsTmBool ty pt TmFBool where
  _TmBoolP = id

valBool :: AsTmBool ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
valBool tm = do
  _ <- preview _TmBool tm
  return tm

stepAnd1 :: AsTmBool ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepAnd1 stepFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  tm1' <- stepFn tm1
  return . review _TmAnd $ (tm1', tm2)

stepAnd2 :: AsTmBool ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepAnd2 stepFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  _ <- preview _TmBool tm1
  tm2' <- stepFn tm2
  return . review _TmAnd $ (tm1, tm2')

stepAndBool :: AsTmBool ty pt tm
           => Term ty pt tm a
           -> Maybe (Term ty pt tm a)
stepAndBool tm = do
  (tm1, tm2) <- preview _TmAnd tm
  b1 <- preview _TmBool tm1
  b2 <- preview _TmBool tm2
  return . review _TmBool $ b1 && b2

stepOr1 :: AsTmBool ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepOr1 stepFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  tm1' <- stepFn tm1
  return . review _TmOr $ (tm1', tm2)

stepOr2 :: AsTmBool ty pt tm
         => (Term ty pt tm a -> Maybe (Term ty pt tm a))
         -> Term ty pt tm a
         -> Maybe (Term ty pt tm a)
stepOr2 stepFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  _ <- preview _TmBool tm1
  tm2' <- stepFn tm2
  return . review _TmOr $ (tm1, tm2')

stepOrBool :: AsTmBool ty pt tm
           => Term ty pt tm a
           -> Maybe (Term ty pt tm a)
stepOrBool tm = do
  (tm1, tm2) <- preview _TmOr tm
  b1 <- preview _TmBool tm1
  b2 <- preview _TmBool tm2
  return . review _TmBool $ b1 || b2

inferBool :: (Monad m, AsTyBool ty, AsTmBool ty pt tm)
         => Term ty pt tm a
         -> Maybe (m (Type ty a))
inferBool tm = do
  _ <- preview _TmBool tm
  return . return . review _TyBool $ ()

inferAnd :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyBool ty, AsTmBool ty pt tm)
         => (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferAnd inferFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  return $ do
    let ty = review _TyBool ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

inferOr :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyBool ty, AsTmBool ty pt tm)
         => (Term ty pt tm a -> m (Type ty a))
         -> Term ty pt tm a
         -> Maybe (m (Type ty a))
inferOr inferFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  return $ do
    let ty = review _TyBool ()
    mkCheck inferFn tm1 ty
    mkCheck inferFn tm2 ty
    return ty

matchBool :: (AsPtBool pt, AsTmBool ty pt tm)
          => (Term ty pt tm a -> Term ty pt tm a)
          -> Pattern pt a
          -> Term ty pt tm a
          -> Maybe [Term ty pt tm a]
matchBool eval p tm = do
  b <- preview _PtBool p
  c <- preview _TmBool (eval tm)
  if b == c
  then return []
  else mzero

checkBool :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsPtBool pt, AsTyBool ty) => Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkBool p ty = do
  _ <- preview _PtBool p
  return $ do
    let tyB = review _TyBool ()
    expect (Expected tyB) (Actual ty)
    return []

type BoolContext e s r m ty pt tm a = (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsTyBool ty, AsPtBool pt, AsTmBool ty pt tm)

boolFragment :: BoolContext e s r m ty pt tm a
            => FragmentInput e s r m ty pt tm a
boolFragment =
  FragmentInput
    [ValueBase valBool]
    [ EvalStep stepAnd1
    , EvalStep stepAnd2
    , EvalBase stepAndBool
    , EvalStep stepOr1
    , EvalStep stepOr2
    , EvalBase stepOrBool
    ]
    [ InferBase inferBool
    , InferRecurse inferAnd
    , InferRecurse inferOr
    ]
    [ PMatchEval matchBool ]
    [ PCheckBase checkBool ]
    [ ]
    [ ]

-- Helpers

tyBool :: AsTyBool ty => Type ty a
tyBool = review _TyBool ()

ptBool :: AsPtBool pt => Bool -> Pattern pt a
ptBool = review _PtBool

tmBool :: AsTmBool ty pt tm => Bool -> Term ty pt tm a
tmBool = review _TmBool

tmAnd :: AsTmBool ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmAnd = curry $ review _TmAnd

tmOr :: AsTmBool ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmOr = curry $ review _TmOr
