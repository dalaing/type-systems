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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Fragment.Tuple (
    TyFTuple(..)
  , AsTyTuple(..)
  , PtFTuple(..)
  , AsPtTuple(..)
  , TmFTuple(..)
  , AsTmTuple(..)
  , AsExpectedTyTuple(..)
  , AsTupleOutOfBounds(..)
  , TupleContext
  , tupleFragmentLazy
  , tupleFragmentStrict
  , tyTuple
  , ptTuple
  , tmTuple
  , tmTupleIx
  ) where

import Control.Monad (zipWithM)
import Data.List (splitAt)
import Data.Foldable (asum)

import Control.Monad.Except (MonadError)

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Fragment.Ast
import Util

data TyFTuple f a =
  TyTupleF [f a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFTuple

deriveEq1 ''TyFTuple
deriveOrd1 ''TyFTuple
deriveShow1 ''TyFTuple

instance EqRec TyFTuple where
  liftEqRec eR _ (TyTupleF xs) (TyTupleF ys) = and $ zipWith eR xs ys

instance OrdRec TyFTuple where
  liftCompareRec _ _ (TyTupleF []) (TyTupleF []) = EQ
  liftCompareRec _ _ (TyTupleF []) (TyTupleF (_ : _)) = LT
  liftCompareRec _ _ (TyTupleF (_ : _)) (TyTupleF []) = GT
  liftCompareRec cR c (TyTupleF (x : xs)) (TyTupleF (y : ys)) =
    case cR x y of
      EQ -> liftCompareRec cR c (TyTupleF xs) (TyTupleF ys)
      z -> z

instance ShowRec TyFTuple where
  liftShowsPrecRec _ slR _ _ n (TyTupleF xs) =
    showsUnaryWith (const slR) "TyTupleF" n xs

instance Bound TyFTuple where
  TyTupleF tys >>>= f = TyTupleF (fmap (>>= f) tys)

instance Bitransversable TyFTuple where
  bitransverse fT fL (TyTupleF tys) = TyTupleF <$> traverse (fT fL) tys

class AsTyTuple ty where
  _TyTupleP :: Prism' (ty k a) (TyFTuple k a)

  _TyTuple :: Prism' (Type ty a) [Type ty a]
  _TyTuple = _TyTree . _TyTupleP . _TyTupleF

instance AsTyTuple TyFTuple where
  _TyTupleP = id

instance {-# OVERLAPPABLE #-} AsTyTuple (TSum xs) => AsTyTuple (TSum (x ': xs)) where
  _TyTupleP = _TNext . _TyTupleP

instance {-# OVERLAPPING #-} AsTyTuple (TSum (TyFTuple ': xs)) where
  _TyTupleP = _TAdd . _TyTupleP

data PtFTuple f a =
  PtTupleF [f a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFTuple

deriveEq1 ''PtFTuple
deriveOrd1 ''PtFTuple
deriveShow1 ''PtFTuple

instance EqRec PtFTuple where
  liftEqRec eR _ (PtTupleF xs) (PtTupleF ys) = and $ zipWith eR xs ys

instance OrdRec PtFTuple where
  liftCompareRec _ _ (PtTupleF []) (PtTupleF []) = EQ
  liftCompareRec _ _ (PtTupleF []) (PtTupleF (_ : _)) = LT
  liftCompareRec _ _ (PtTupleF (_ : _)) (PtTupleF []) = GT
  liftCompareRec cR c (PtTupleF (x : xs)) (PtTupleF (y : ys)) =
    case cR x y of
      EQ -> liftCompareRec cR c (PtTupleF xs) (PtTupleF ys)
      z -> z

instance ShowRec PtFTuple where
  liftShowsPrecRec _ slR _ _ n (PtTupleF xs) =
    showsUnaryWith (const slR) "PtPupleF" n xs

instance Bound PtFTuple where
  PtTupleF pts >>>= f = PtTupleF (fmap (>>= f) pts)

instance Bitransversable PtFTuple where
  bitransverse fT fL (PtTupleF pts) = PtTupleF <$> traverse (fT fL) pts

class AsPtTuple pt where
  _PtTupleP :: Prism' (pt k a) (PtFTuple k a)

  _PtTuple :: Prism' (Pattern pt a) [Pattern pt a]
  _PtTuple = _PtTree . _PtTupleP . _PtTupleF

instance AsPtTuple PtFTuple where
  _PtTupleP = id

instance {-# OVERLAPPABLE #-} AsPtTuple (TSum xs) => AsPtTuple (TSum (x ': xs)) where
  _PtTupleP = _TNext . _PtTupleP

instance {-# OVERLAPPING #-} AsPtTuple (TSum (PtFTuple ': xs)) where
  _PtTupleP = _TAdd . _PtTupleP

data TmFTuple (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmTupleF [f a]
  | TmTupleIxF (f a) Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFTuple

deriveEq1 ''TmFTuple
deriveOrd1 ''TmFTuple
deriveShow1 ''TmFTuple

instance EqRec (TmFTuple ty pt) where
  liftEqRec eR _ (TmTupleF xs) (TmTupleF ys) =
    and $ zipWith eR xs ys
  liftEqRec eR _ (TmTupleIxF x1 i1) (TmTupleIxF x2 i2) =
    eR x1 x2 && i1 == i2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFTuple ty pt) where
  liftCompareRec _ _ (TmTupleF []) (TmTupleF []) = EQ
  liftCompareRec _ _ (TmTupleF []) (TmTupleF (_ : _)) = LT
  liftCompareRec _ _ (TmTupleF (_ : _)) (TmTupleF []) = GT
  liftCompareRec cR c (TmTupleF (x : xs)) (TmTupleF (y : ys)) =
    case cR x y of
      EQ -> liftCompareRec cR c (TmTupleF xs) (TmTupleF ys)
      z -> z
  liftCompareRec _ _ (TmTupleF _) _ = LT
  liftCompareRec _ _ _ (TmTupleF _) = GT
  liftCompareRec cR _ (TmTupleIxF x1 i1) (TmTupleIxF x2 i2) =
    case cR x1 x2 of
      EQ -> compare i1 i2
      z -> z

instance ShowRec (TmFTuple ty pt) where
  liftShowsPrecRec _ slR _ _ n (TmTupleF xs) =
    showsUnaryWith (const slR) "TmTupleF" n xs
  liftShowsPrecRec sR _ _ _ n (TmTupleIxF x i) =
    showsBinaryWith sR showsPrec "TmTupleIxF" n x i

instance Bound (TmFTuple ty pt) where
  TmTupleF tms >>>= f = TmTupleF (fmap (>>= f) tms)
  TmTupleIxF tm i >>>= f = TmTupleIxF (tm >>= f) i

instance Bitransversable (TmFTuple ty tp) where
  bitransverse fT fL (TmTupleF tms) = TmTupleF <$> traverse (fT fL) tms
  bitransverse fT fL (TmTupleIxF tm i) = TmTupleIxF <$> fT fL tm <*> pure i

class AsTmTuple ty pt tm where
  _TmTupleP :: Prism' (tm ty pt k a) (TmFTuple ty pt k a)

  _TmTuple :: Prism' (Term ty pt tm a) [Term ty pt tm a]
  _TmTuple = _Wrapped . _ATerm . _TmTupleP . _TmTupleF . mapping _Unwrapped

  _TmTupleIx :: Prism' (Term ty pt tm a) (Term ty pt tm a, Int)
  _TmTupleIx = _Wrapped . _ATerm . _TmTupleP . _TmTupleIxF . bimapping _Unwrapped id

instance AsTmTuple ty pt TmFTuple where
  _TmTupleP = id

-- Errors

class AsExpectedTyTuple e ty a | e -> ty, e -> a where
  _ExpectedTyTuple :: Prism' e (Type ty a)

expectTyTuple :: (MonadError e m, AsExpectedTyTuple e ty a, AsTyTuple ty) => Type ty a -> m [Type ty a]
expectTyTuple ty =
  case preview _TyTuple ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyTuple ty

class AsTupleOutOfBounds e where
  _TupleOutOfBounds :: Prism' e (Int, Int)

lookupTuple :: (MonadError e m, AsTupleOutOfBounds e) =>  [t a] -> Int -> m (t a)
lookupTuple ts i =
  let
    l = length ts
    f x
      | x < 0 = throwing _TupleOutOfBounds (x, l)
      | x >= l = throwing _TupleOutOfBounds (x, l)
      | otherwise = return $ ts !! i
  in
    f i

-- Rules

stepTupleIxLazy :: AsTmTuple ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTupleIxLazy tm = do
  (tmT ,i) <- preview _TmTupleIx tm
  tms <- preview _TmTuple tmT
  return $ tms !! i

-- TODO check this, there might be more rules
evalRulesLazy :: AsTmTuple ty pt tm => FragmentInput e s r m ty pt tm a
evalRulesLazy =
  FragmentInput [] [EvalBase stepTupleIxLazy] [] [] [] [] []

valueTuple :: AsTmTuple ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
valueTuple valueFn tm = do
  tms <- preview _TmTuple tm
  vs <- traverse valueFn tms
  return $ review _TmTuple vs

stepTupleIxStrict :: AsTmTuple ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTupleIxStrict stepFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  tmT' <- stepFn tmT
  return $ review _TmTupleIx (tmT', i)

stepTupleElimIxStrict :: AsTmTuple ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTupleElimIxStrict valueFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  tms <- preview _TmTuple tmT
  vs <- traverse valueFn tms
  return $ vs !! i

stepTupleIx :: AsTmTuple ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Int -> Maybe (Term ty pt tm a)
stepTupleIx valueFn stepFn tm i = do
  tms <- preview _TmTuple tm
  let (vs, s : ts) = splitAt i tms
  vs' <- traverse valueFn vs
  s' <- stepFn s
  return $ review _TmTuple (vs' ++ s' : ts)

stepTuple :: AsTmTuple ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepTuple valueFn stepFn tm = do
  tms <- preview _TmTuple tm
  let l = length tms
  asum . fmap (stepTupleIx valueFn stepFn tm) $ [0..l-1]

evalRulesStrict :: AsTmTuple ty pt tm => FragmentInput e s r m ty pt tm a
evalRulesStrict =
  FragmentInput
    [ ValueRecurse valueTuple ]
    [ EvalStep stepTupleIxStrict
    , EvalValue stepTupleElimIxStrict
    , EvalValueStep stepTuple
    ]
    [] [] [] [] []

inferTmTuple :: (Monad m, AsTyTuple ty, AsTmTuple ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmTuple inferFn tm = do
  tms <- preview _TmTuple tm
  return $ do
    tys <- traverse inferFn tms
    return $ review _TyTuple tys

inferTmTupleIx :: (MonadError e m, AsExpectedTyTuple e ty a, AsTupleOutOfBounds e, AsTyTuple ty, AsTmTuple ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmTupleIx inferFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectTyTuple tyT
    lookupTuple tys i

inferRules :: (MonadError e m, AsExpectedTyTuple e ty a, AsTupleOutOfBounds e, AsTyTuple ty, AsTmTuple ty pt tm) => FragmentInput e s r m ty pt tm a
inferRules =
  FragmentInput
    [] []
    [ InferRecurse inferTmTuple
    , InferRecurse inferTmTupleIx
    ]
    [] [] [] []

matchTuple :: (AsPtTuple pt, AsTmTuple ty pt tm) => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
matchTuple matchFn p tm = do
  pts <- preview _PtTuple p
  tms <- preview _TmTuple tm
  tmss <- zipWithM matchFn pts tms
  return $ mconcat tmss

checkTuple :: (MonadError e m, AsExpectedTyTuple e ty a, AsTyTuple ty, AsPtTuple pt) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkTuple checkFn p ty = do
  pts <- preview _PtTuple p
  return $ do
    tys <- expectTyTuple ty
    ms <- zipWithM checkFn pts tys
    return $ mconcat ms

patternRules :: (MonadError e m, AsExpectedTyTuple e ty a, AsTyTuple ty, AsPtTuple pt, AsTmTuple ty pt tm) => FragmentInput e s r m ty pt tm a
patternRules =
  FragmentInput
    [] [] [] [ PMatchRecurse matchTuple ] [ PCheckRecurse checkTuple ] [] []

type TupleContext e s r m ty pt tm a = (MonadError e m, AsExpectedTyTuple e ty a, AsTupleOutOfBounds e, AsTyTuple ty, AsPtTuple pt, AsTmTuple ty pt tm)

tupleFragmentLazy :: TupleContext e s r m ty pt tm a
             => FragmentInput e s r m ty pt tm a
tupleFragmentLazy =
  evalRulesLazy `mappend` inferRules `mappend` patternRules

tupleFragmentStrict :: TupleContext e s r m ty pt tm a
             => FragmentInput e s r m ty pt tm a
tupleFragmentStrict =
  evalRulesStrict `mappend` inferRules `mappend` patternRules

-- Helpers

tyTuple :: AsTyTuple ty => [Type ty a] -> Type ty a
tyTuple = review _TyTuple

ptTuple :: AsPtTuple pt => [Pattern pt a] -> Pattern pt a
ptTuple = review _PtTuple

tmTuple :: AsTmTuple ty pt tm => [Term ty pt tm a] -> Term ty pt tm a
tmTuple = review _TmTuple

tmTupleIx :: AsTmTuple ty pt tm => Term ty pt tm a -> Int -> Term ty pt tm a
tmTupleIx = curry $ review _TmTupleIx
