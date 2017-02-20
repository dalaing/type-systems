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
{-# LANGUAGE RankNTypes #-}
module Fragment.If (
    TmFIf(..)
  , AsTmIf(..)
  , IfContext
  , ifFragment
  , tmIf
  ) where

import Control.Monad.Except (MonadError)


import Control.Lens

import Bound

import Data.Deriving

import Fragment
import Fragment.Ast
import Fragment.Bool
import Error
import Util

data TmFIf (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmIfF (f a) (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFIf

deriveEq1 ''TmFIf
deriveOrd1 ''TmFIf
deriveShow1 ''TmFIf

instance EqRec (TmFIf ty pt) where
  liftEqRec eR _ (TmIfF x1 y1 z1) (TmIfF x2 y2 z2) =
    eR x1 x2 && eR y1 y2 && eR z1 z2

instance OrdRec (TmFIf ty pt) where
  liftCompareRec cR _ (TmIfF x1 y1 z1) (TmIfF x2 y2 z2) =
    case cR x1 x2 of
      EQ -> case cR y1 y2 of
        EQ -> cR z1 z2
        z -> z
      z -> z

instance ShowRec (TmFIf ty pt) where
  liftShowsPrecRec sR _ _ _ n (TmIfF x y z) =
    showString "if " .
    sR n x .
    showString " then " .
    sR n y .
    showString " else " .
    sR n z

instance Bound (TmFIf ty pt) where
  TmIfF b t e >>>= f = TmIfF (b >>= f) (t >>= f) (e >>= f)

instance Bitransversable (TmFIf ty pt) where
  bitransverse fT fL (TmIfF x y z) = TmIfF <$> fT fL x <*> fT fL y <*> fT fL z

class AsTmIf ty pt tm where
  _TmIfP :: Prism' (tm ty pt k a) (TmFIf ty pt k a)

  _TmIf :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a, Term ty pt tm a)
  _TmIf = _Wrapped . _ATerm . _TmIfP . _TmIfF . mkTriple _Unwrapped _Unwrapped _Unwrapped

instance AsTmIf ty pt TmFIf where
  _TmIfP = id

-- Rules

stepIf1 :: AsTmIf ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepIf1 stepFn tm = do
  (tmB, tmT, tmE) <- preview _TmIf tm
  tmB' <- stepFn tmB
  return $ review _TmIf (tmB', tmT, tmE)

stepIf2 :: (AsTmIf ty pt tm, AsTmBool ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepIf2 valueFn tm = do
  (tmB, tmT, tmF) <- preview _TmIf tm
  vB <- valueFn tmB
  b <- preview _TmBool vB
  return $
    if b then tmT else tmF

inferTmIf :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsExpectedEq e ty a, AsTyBool ty, AsTmIf ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmIf inferFn tm = do
  (tmB, tmT, tmF) <- preview _TmIf tm
  return $ do
    tyB <- inferFn tmB
    expect (Expected tyB) (Actual (review _TyBool ()))
    tyT <- inferFn tmT
    tyF <- inferFn tmF
    expectEq tyT tyF
    return tyT

type IfContext e s r m ty pt tm a = (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsExpectedEq e ty a, AsTyBool ty, AsTmIf ty pt tm, AsTmBool ty pt tm)

ifFragment :: IfContext e s r m ty pt tm a
             => FragmentInput e s r m ty pt tm a
ifFragment =
  FragmentInput []
    [ EvalStep stepIf1, EvalValue stepIf2]
    [ InferRecurse inferTmIf ]
    [] [] [] []

-- Helpers

tmIf :: AsTmIf ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmIf b t f = review _TmIf (b, t, f)
