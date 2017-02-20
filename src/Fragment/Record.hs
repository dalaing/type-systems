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
module Fragment.Record (
    TyFRecord(..)
  , AsTyRecord(..)
  , PtFRecord(..)
  , AsPtRecord(..)
  , TmFRecord(..)
  , AsTmRecord(..)
  , AsExpectedTyRecord(..)
  , AsRecordNotFound(..)
  , RecordContext
  , recordFragmentLazy
  , recordFragmentStrict
  , tyRecord
  , ptRecord
  , tmRecord
  , tmRecordIx
  ) where

import Data.List (splitAt)
import Data.Foldable (asum)
import Text.Show

import Control.Monad.Except (MonadError)

import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens (throwing)

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Fragment.Ast
import Util

data TyFRecord f a =
  TyRecordF [(T.Text, f a)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFRecord

deriveEq1 ''TyFRecord
deriveOrd1 ''TyFRecord
deriveShow1 ''TyFRecord

instance EqRec TyFRecord where
  liftEqRec eR _ (TyRecordF xs) (TyRecordF ys) =
    let
      f (l1, x1) (l2, x2) = l1 == l2 && eR x1 x2
    in
      and $ zipWith f xs ys

instance OrdRec TyFRecord where
  liftCompareRec _ _ (TyRecordF []) (TyRecordF []) = EQ
  liftCompareRec _ _ (TyRecordF []) (TyRecordF (_ : _)) = LT
  liftCompareRec _ _ (TyRecordF (_ : _)) (TyRecordF []) = GT
  liftCompareRec cR c (TyRecordF ((lx, x) : xs)) (TyRecordF ((ly, y) : ys)) =
    case compare lx ly of
      EQ -> case cR x y of
        EQ -> liftCompareRec cR c (TyRecordF xs) (TyRecordF ys)
        z -> z
      z -> z

instance ShowRec TyFRecord where
  liftShowsPrecRec sR _ _ _ n (TyRecordF xs) =
    let
      g m (l, x) = showString ("(" ++ T.unpack l ++ ", ") .
                 sR m x .
                 showString ")"
      f _ ps = showListWith (g 0) ps
    in
      showsUnaryWith f "TyRecordF" n xs

instance Bound TyFRecord where
  TyRecordF tys >>>= f = TyRecordF (fmap (fmap (>>= f)) tys)

instance Bitransversable TyFRecord where
  bitransverse fT fL (TyRecordF rs) = TyRecordF <$> traverse (traverse (fT fL)) rs

class AsTyRecord ty where
  _TyRecordP :: Prism' (ty k a) (TyFRecord k a)

  _TyRecord :: Prism' (Type ty a) [(T.Text, Type ty a)]
  _TyRecord = _TyTree . _TyRecordP . _TyRecordF

instance AsTyRecord TyFRecord where
  _TyRecordP = id

data PtFRecord pt a =
    PtRecordF [(T.Text, pt a)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFRecord

deriveEq1 ''PtFRecord
deriveOrd1 ''PtFRecord
deriveShow1 ''PtFRecord

instance Bound PtFRecord where
  PtRecordF pts >>>= f = PtRecordF (fmap (fmap (>>= f)) pts)

instance Bitransversable PtFRecord where
  bitransverse fT fL (PtRecordF rs) = PtRecordF <$> traverse (traverse (fT fL)) rs

class AsPtRecord pt where
  _PtRecordP :: Prism' (pt k a) (PtFRecord k a)

  _PtRecord :: Prism' (Pattern pt a) [(T.Text, Pattern pt a)]
  _PtRecord = _PtTree . _PtRecordP . _PtRecordF

instance AsPtRecord PtFRecord where
  _PtRecordP = id

data TmFRecord (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmRecordF [(T.Text, f a)]
  | TmRecordIxF (f a) T.Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFRecord

deriveEq1 ''TmFRecord
deriveOrd1 ''TmFRecord
deriveShow1 ''TmFRecord

instance Bound (TmFRecord ty pt) where
  TmRecordF tms >>>= f = TmRecordF (fmap (fmap (>>= f)) tms)
  TmRecordIxF tm t >>>= f = TmRecordIxF (tm >>= f) t

instance Bitransversable (TmFRecord ty pt) where
  bitransverse fT fL (TmRecordF rs) = TmRecordF <$> traverse (traverse (fT fL)) rs
  bitransverse fT fL (TmRecordIxF r t) = TmRecordIxF <$> fT fL r <*> pure t

class AsTmRecord ty pt tm where
  _TmRecordP :: Prism' (tm ty pt k a) (TmFRecord ty pt k a)

  _TmRecord :: Prism' (Term ty pt tm a) [(T.Text, Term ty pt tm a)]
  _TmRecord = _Wrapped . _ATerm . _TmRecordP . _TmRecordF . mapping (seconding _Unwrapped)

  _TmRecordIx :: Prism' (Term ty pt tm a) (Term ty pt tm a, T.Text)
  _TmRecordIx = _Wrapped . _ATerm . _TmRecordP . _TmRecordIxF . firsting _Unwrapped

instance AsTmRecord ty pt TmFRecord where
  _TmRecordP = id

-- Errors

class AsExpectedTyRecord e ty a | e -> ty, e -> a where
  _ExpectedTyRecord :: Prism' e (Type ty a)

expectTyRecord :: (MonadError e m, AsExpectedTyRecord e ty a, AsTyRecord ty) => Type ty a -> m [(T.Text, Type ty a)]
expectTyRecord ty =
  case preview _TyRecord ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyRecord ty

class AsRecordNotFound e where
  _RecordNotFound :: Prism' e T.Text

lookupRecord :: (MonadError e m, AsRecordNotFound e) =>  [(T.Text, t a)] -> T.Text -> m (t a)
lookupRecord ts t =
  case lookup t ts of
    Just x -> return x
    Nothing -> throwing _RecordNotFound t

-- Rules

stepRecordIxLazy :: AsTmRecord ty pt tm => Term ty pt tm a -> Maybe (Term ty pt tm a)
stepRecordIxLazy tm = do
  (tmT, t) <- preview _TmRecordIx tm
  tms <- preview _TmRecord tmT
  let Just tm' = lookup t tms
  return tm'

-- TODO check this, there might be more rules
evalRulesLazy :: AsTmRecord ty pt tm => FragmentInput e s r m ty pt tm a
evalRulesLazy =
  FragmentInput [] [EvalBase stepRecordIxLazy] [] [] [] [] []

valueRecord :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
valueRecord valueFn tm = do
  tms <- preview _TmRecord tm
  vs <- traverse (traverse valueFn) tms
  return $ review _TmRecord vs

stepRecordIxStrict :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepRecordIxStrict stepFn tm = do
  (tmR, t) <- preview _TmRecordIx tm
  tmR' <- stepFn tmR
  return $ review _TmRecordIx (tmR', t)

stepRecordElimIxStrict :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepRecordElimIxStrict valueFn tm = do
  (tmR, t) <- preview _TmRecordIx tm
  tms <- preview _TmRecord tmR
  vs <- traverse (traverse valueFn) tms
  let Just v = lookup t vs
  return v

stepRecordIx :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Int -> Maybe (Term ty pt tm a)
stepRecordIx valueFn stepFn tm i = do
  tms <- preview _TmRecord tm
  let (vs, s : ts) = splitAt i tms
  vs' <- traverse (traverse valueFn) vs
  s' <- traverse stepFn s
  return $ review _TmRecord (vs' ++ s' : ts)

stepRecord :: AsTmRecord ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepRecord valueFn stepFn tm = do
  tms <- preview _TmRecord tm
  let l = length tms
  asum . fmap (stepRecordIx valueFn stepFn tm) $ [0..l-1]

evalRulesStrict :: AsTmRecord ty pt tm => FragmentInput e s r m ty pt tm a
evalRulesStrict =
  FragmentInput
    [ ValueRecurse valueRecord ]
    [ EvalStep stepRecordIxStrict
    , EvalValue stepRecordElimIxStrict
    , EvalValueStep stepRecord
    ]
    [] [] [] [] []

inferTmRecord :: (Monad m, AsTyRecord ty, AsTmRecord ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmRecord inferFn tm = do
  tms <- preview _TmRecord tm
  return $ do
    tys <- traverse (traverse inferFn) tms
    return $ review _TyRecord tys

inferTmRecordIx :: (MonadError e m, AsExpectedTyRecord e ty a, AsRecordNotFound e, AsTyRecord ty, AsTmRecord ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmRecordIx inferFn tm = do
  (tmT, i) <- preview _TmRecordIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectTyRecord tyT
    lookupRecord tys i

matchRecord :: (AsPtRecord pt, AsTmRecord ty pt tm) => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
matchRecord matchFn p tm = do
  ps <- preview _PtRecord p
  ltms <- preview _TmRecord tm
  let f (l, lp) = do
        tmp <- lookup l ltms
        matchFn lp tmp
  fmap mconcat . traverse f $ ps

checkRecord :: (MonadError e m, AsExpectedTyRecord e ty a, AsRecordNotFound e, AsPtRecord pt, AsTyRecord ty) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkRecord checkFn p ty = do
  ps <- preview _PtRecord p
  return $ do
    -- check for duplicate labels in ps
    ltys <- expectTyRecord ty
    let f (l, lp) = do
          typ <- lookupRecord ltys l
          checkFn lp typ
    fmap mconcat . traverse f $ ps

type RecordContext e s r m ty pt tm a = (MonadError e m, AsExpectedTyRecord e ty a, AsRecordNotFound e, AsTyRecord ty, AsPtRecord pt, AsTmRecord ty pt tm)

baseRules :: RecordContext e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
baseRules =
  FragmentInput
    []
    []
    [ InferRecurse inferTmRecord
    , InferRecurse inferTmRecordIx
    ]
    [ PMatchRecurse matchRecord ]
    [ PCheckRecurse checkRecord ]
    [] []

recordFragmentLazy :: RecordContext e s r m ty pt tm a
             => FragmentInput e s r m ty pt tm a
recordFragmentLazy =
  mappend evalRulesLazy baseRules

recordFragmentStrict :: RecordContext e s r m ty pt tm a
             => FragmentInput e s r m ty pt tm a
recordFragmentStrict =
  mappend evalRulesStrict baseRules

-- Helpers

tyRecord :: AsTyRecord ty => [(T.Text, Type ty a)] -> Type ty a
tyRecord = review _TyRecord

ptRecord :: AsPtRecord pt => [(T.Text, Pattern pt a)] -> Pattern pt a
ptRecord = review _PtRecord

tmRecord :: AsTmRecord ty pt tm => [(T.Text, Term ty pt tm a)] -> Term ty pt tm a
tmRecord = review _TmRecord

tmRecordIx :: AsTmRecord ty pt tm => Term ty pt tm a -> T.Text -> Term ty pt tm a
tmRecordIx = curry $ review _TmRecordIx
