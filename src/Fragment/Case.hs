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
module Fragment.Case (
    Alt(..)
  , TmFCase(..)
  , AsTmCase(..)
  , AsExpectedPattern(..)
  , AsDuplicatedPatternVariables(..)
  , AsUnusedPatternVariables(..)
  , CaseContext
  , caseFragmentLazy
  , caseFragmentStrict
  , tmAlt
  , tmCase
  ) where

import Data.List (elemIndex, sort, group, (\\))
import Data.Foldable (toList)
import Control.Monad (replicateM)

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N

import Control.Lens
import Control.Monad.Error.Lens

import Bound
import Bound.Scope
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Fragment.Ast
import Fragment.Var
import Error
import Util

data Alt (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) k a =
  Alt (k a) (Scope Int k a)
  deriving (Functor, Foldable, Traversable)

makePrisms ''Alt

instance (Eq1 tm, Monad tm) => Eq1 (Alt ty pt tm) where
  liftEq = $(makeLiftEq ''Alt)

instance (Ord1 tm, Monad tm) => Ord1 (Alt ty pt tm) where
  liftCompare = $(makeLiftCompare ''Alt)

instance (Show1 tm) => Show1 (Alt ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''Alt)

instance (Eq a, Eq1 tm, Monad tm) => Eq (Alt ty pt tm a) where (==) = eq1
instance (Ord a, Ord1 tm, Monad tm) => Ord (Alt ty pt tm a) where compare = compare1
instance (Show a, Show1 tm) => Show (Alt ty pt tm a) where showsPrec = showsPrec1

instance EqRec (Alt ty pt) where
  liftEqRec eR e (Alt pt1 tm1) (Alt pt2 tm2) =
    eR pt1 pt2 && liftEqRec eR e tm1 tm2

instance OrdRec (Alt ty pt) where
  liftCompareRec cR c (Alt pt1 tm1) (Alt pt2 tm2) =
    case cR pt1 pt2 of
      EQ -> liftCompareRec cR c tm1 tm2
      z -> z

instance ShowRec (Alt ty pt) where
  liftShowsPrecRec sR slR s sl n (Alt pt tm) =
    showsBinaryWith sR (liftShowsPrecRec sR slR s sl) "Alt" n pt tm

instance Bound (Alt ty pt) where
  Alt pt s >>>= f = Alt (pt >>= f) (s >>>= f)

instance Bitransversable (Alt ty pt) where
  bitransverse fT fL (Alt pt s) = Alt <$> fT fL pt <*> bitransverse fT fL s

data TmFCase (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmCaseF (f a) (N.NonEmpty (Alt ty pt f a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFCase

deriveEq1 ''NonEmpty
deriveOrd1 ''NonEmpty
deriveShow1 ''NonEmpty

instance (Eq1 tm, Monad tm) => Eq1 (TmFCase ty pt tm) where
  liftEq = $(makeLiftEq ''TmFCase)

instance (Ord1 tm, Monad tm) => Ord1 (TmFCase ty pt tm) where
  liftCompare = $(makeLiftCompare ''TmFCase)

instance (Show1 tm) => Show1 (TmFCase ty pt tm) where
  liftShowsPrec = $(makeLiftShowsPrec ''TmFCase)

instance EqRec (TmFCase ty pt) where
  liftEqRec eR e (TmCaseF tm1 alts1) (TmCaseF tm2 alts2) =
    eR tm1 tm2 && and (N.zipWith (liftEqRec eR e) alts1 alts2)

instance OrdRec (TmFCase ty pt) where
  liftCompareRec cR c (TmCaseF tm1 alts1) (TmCaseF tm2 alts2) =
    let
      f [] [] = EQ
      f [] _ = LT
      f _ [] = GT
      f (x : xs) (y : ys) =
        case liftCompareRec cR c x y of
          EQ -> f xs ys
          z -> z
    in
      case cR tm1 tm2 of
        EQ -> f (N.toList alts1) (N.toList alts2)
        z -> z

instance ShowRec (TmFCase ty pt) where
  liftShowsPrecRec sR slR s sl n (TmCaseF tm alts) =
    showsBinaryWith sR (\_ -> liftShowListRec sR slR s sl) "TmCaseF" n tm (N.toList alts)

instance Bound (TmFCase ty pt) where
  TmCaseF tm alts >>>= f = TmCaseF (tm >>= f) (fmap (>>>= f) alts)

instance Bitransversable (TmFCase ty tp) where
  bitransverse fT fL (TmCaseF tm alts) = TmCaseF <$> fT fL tm <*> traverse (bitransverse fT fL) alts

class AsTmCase ty pt tm where
  _TmCaseP :: Prism' (tm ty pt k a) (TmFCase ty pt k a)

  _TmCase :: Prism' (Term ty pt tm a) (Term ty pt tm a, N.NonEmpty (Alt ty pt (Ast ty pt tm) (AstVar a)))
  _TmCase = _Wrapped . _ATerm. _TmCaseP . _TmCaseF . firsting _Unwrapped

instance AsTmCase ty pt TmFCase where
  _TmCaseP = id

-- Errors

class AsExpectedPattern e ty pt tm a | e -> ty, e -> pt, e -> tm, e -> a where
  _ExpectedPattern :: Prism' e (Ast ty pt tm (AstVar a))

expectPattern :: (MonadError e m, AsExpectedPattern e ty pt tm a, AstTransversable ty pt tm) => Ast ty pt tm (AstVar a) -> m (Pattern pt a)
expectPattern ast =
  case preview _Pattern ast of
    Just p -> return p
    _ -> throwing _ExpectedPattern ast

class AsDuplicatedPatternVariables e a | e -> a where
  _DuplicatedPatternVariables :: Prism' e (N.NonEmpty a)

checkForDuplicatedPatternVariables :: (Ord a, MonadError e m, AsDuplicatedPatternVariables e a) => [a] -> m ()
checkForDuplicatedPatternVariables xs =
  let
    dups = map head .
           filter ((> 1) . length) .
           group .
           sort $
           xs
  in
    case N.nonEmpty dups of
      Nothing -> return ()
      Just ns -> throwing _DuplicatedPatternVariables ns

class AsUnusedPatternVariables e a | e -> a where
  _UnusedPatternVariables :: Prism' e (N.NonEmpty a)

checkForUnusedPatternVariables :: (MonadError e m, AsUnusedPatternVariables e a) => [a] -> [Int] -> m ()
checkForUnusedPatternVariables vs is =
  let
    n = length vs
    unusedI = [0..(n - 1)] \\ is
    unusedV = map (vs !!) unusedI
  in
    case N.nonEmpty unusedV of
      Nothing -> return ()
      Just ns -> throwing _UnusedPatternVariables ns

-- Rules


type MatchConstraint ty pt tm = (AstTransversable ty pt tm, AstBound ty pt tm)

handleMatch :: MatchConstraint ty pt tm
            => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])
            -> Term ty pt tm a -> N.NonEmpty (Alt ty pt (Ast ty pt tm ) (AstVar a))
            -> Maybe (Term ty pt tm a)
handleMatch matchFn tm alts =
  let
    go (Alt p s : rest) = do
      p' <- preview _Pattern p
      case matchFn p' tm of
        Nothing -> go rest
        Just tms -> return . review _Wrapped . instantiate (review _Unwrapped . (tms !!)) $ s
    go [] = Nothing
  in
    go (N.toList alts)

stepCaseLazy :: (MatchConstraint ty pt tm, AsTmCase ty pt tm) => (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepCaseLazy matchFn tm = do
  (tmC, alts) <- preview _TmCase tm
  handleMatch matchFn tmC alts

evalRulesLazy :: (MatchConstraint ty pt tm, AsTmCase ty pt tm) => FragmentInput e s r m ty pt tm a
evalRulesLazy =
  FragmentInput []
    [EvalPMatch stepCaseLazy]
    [] [] [] [] []

stepCaseStepStrict :: AsTmCase ty pt tm => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepCaseStepStrict stepFn tm = do
  (tmC, alts) <- preview _TmCase tm
  tmC' <- stepFn tmC
  return $ review _TmCase (tmC', alts)

stepCaseValueStrict :: (MatchConstraint ty pt tm, AsTmCase ty pt tm) => (Term ty pt tm a -> Maybe (Term ty pt tm a)) -> (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Term ty pt tm a -> Maybe (Term ty pt tm a)
stepCaseValueStrict valueFn matchFn tm = do
  (tmC, alts) <- preview _TmCase tm
  vC <- valueFn tmC
  handleMatch matchFn vC alts

evalRulesStrict :: (MatchConstraint ty pt tm, AsTmCase ty pt tm) => FragmentInput e s r m ty pt tm a
evalRulesStrict =
  FragmentInput []
    [EvalStep stepCaseStepStrict, EvalValuePMatch stepCaseValueStrict]
    [] [] [] [] []

type CheckConstraint e s r m ty pt tm a = (Ord a, EqRec ty, MonadError e m, AsExpectedPattern e ty pt tm a, AsExpectedAllEq e ty a, AsDuplicatedPatternVariables e a, AsUnusedPatternVariables e a, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a a, AstBound ty pt tm, AstTransversable ty pt tm, AsTmCase ty pt tm)

inferCase :: CheckConstraint e s r m ty pt tm a => (Term ty pt tm a -> m (Type ty a)) -> (Pattern pt a -> Type ty a -> m [Type ty a]) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferCase inferFn checkFn tm = do
  (tmC, alts) <- preview _TmCase tm
  return $ do
    let go ty (Alt p s) = do
          p' <- expectPattern p

          let vp = toList p'
          checkForDuplicatedPatternVariables vp
          -- TODO possibly turn this into a warning
          checkForUnusedPatternVariables vp (bindings s)

          vs <- replicateM (length p') freshTmVar
          tys <- checkFn p' ty
          let setup = foldr (.) id (zipWith insertTerm vs tys)

          let tm' = review _Wrapped .
                    instantiate (review (_Unwrapped . _TmVar) . (vs !!)) $
                    s
          local (termContext %~ setup) $ inferFn tm'
    tyC <- inferFn tmC
    tys <- mapM (go tyC) alts
    expectAllEq tys

inferRules :: CheckConstraint e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
inferRules =
  FragmentInput [] []
    [InferPCheck inferCase]
    [] [] [] []

type CaseContext e s r m ty pt tm a = (Eq a, MonadError e m, AsTmCase ty pt tm, MatchConstraint ty pt tm, CheckConstraint e s r m ty pt tm a)

caseFragmentLazy :: CaseContext e s r m ty pt tm a
                 => FragmentInput e s r m ty pt tm a
caseFragmentLazy =
  evalRulesLazy `mappend` inferRules

caseFragmentStrict :: CaseContext e s r m ty pt tm a
                   => FragmentInput e s r m ty pt tm a
caseFragmentStrict =
  evalRulesStrict `mappend` inferRules

-- Helpers

tmAlt :: (Eq a, AstBound ty pt tm, AstTransversable ty pt tm) => Pattern pt a -> Term ty pt tm a -> Alt ty pt (Ast ty pt tm) (AstVar a)
tmAlt p tm = Alt (review _Pattern p) s
  where
    vs = fmap ATmVar . toList $ p
    s = abstract (`elemIndex` vs) . review _Unwrapped $ tm

tmCase :: (AsTmCase ty pt tm) => Term ty pt tm a -> [Alt ty pt (Ast ty pt tm) (AstVar a)] -> Term ty pt tm a
tmCase tm alts =
  case N.nonEmpty alts of
    Nothing -> tm
    Just xs -> review _TmCase (tm, xs)
