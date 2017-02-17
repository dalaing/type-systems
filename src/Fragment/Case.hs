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
  , CaseContext
  , caseFragmentLazy
  , caseFragmentStrict
  , tmAlt
  , tmCase
  ) where

import Data.List (elemIndex)
import Data.Foldable (toList)

import Control.Monad.Error (MonadError)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Fragment.Ast
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

-- Rules


type MatchConstraint ty pt tm = (Bound ty, Bound pt, Bound (tm ty pt), TripleConstraint1 Traversable ty pt tm, Traversable (pt (Pattern pt)), Bitransversable pt)

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
    [] [] []

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
    [] [] []

{-
dodgyCheck :: (tm T.Text -> m (ty T.Text)) -> (p T.Text -> ty T.Text -> m [ty T.Text]) -> TmFCase p tm T.Text -> m (ty T.Text)
dodgyCheck inferFn checkFn (TmCaseF tm alts) =
  -- need to check there are no duplicate ptvars per pattern
  -- should also warn if there are unused ptvars per pattern
  -- later: would be good to check for incomplete / unreachable / redundant patterns
  let
    go ty (Alt p s) = do
      tys <- checkFn p ty
      -- TODO generate fresh vars, add tys to context with those vars,
      -- add vars to s, and infer
      infer _
  in
    do
      ty <- inferFn tm
      tys <- mapM (go ty) (N.toList alts)
      expectAllEq tys
-}

{-
inferCase :: AsTmCase ty pt tm => (Term ty pt tm a -> m (Type ty a)) -> (Pattern pt a -> Type ty a -> m [Type ty a]) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferCase inferFn checkFn tm = do
  (tmC, alts) <- preview _TmCase tm
  return $ do
    let go ty (Alt p s) = do
          -- let p' <- preview _Pattern p
          -- if can't convert p to a pattern, explode
          p' <- expectPattern p
          -- check that there are no duplicates in the pattern variables
          -- warn if there are pattern variables that are not used in s
          tys <- checkFn p' ty
          -- generate fresh vars, add the tys to the context with those vars, instantiate s with those vars and infer
          return undefined
    tyC <- inferFn tmC
    tys <- mapM (go tyC) alts
    expectAllEq tys
-}

inferRules :: AsTmCase ty pt tm => FragmentInput e s r m ty pt tm a
inferRules =
  FragmentInput [] []
    [] -- [InferPCheck inferCase]
    [] []

type CaseContext e s r m ty pt tm a = (Eq a, MonadError e m, AsTmCase ty pt tm, MatchConstraint ty pt tm)

caseFragmentLazy :: CaseContext e s r m ty pt tm a
                 => FragmentInput e s r m ty pt tm a
caseFragmentLazy =
  evalRulesLazy `mappend` inferRules

caseFragmentStrict :: CaseContext e s r m ty pt tm a
                   => FragmentInput e s r m ty pt tm a
caseFragmentStrict =
  evalRulesStrict `mappend` inferRules

-- Helpers

tmAlt :: (Eq a, Bound pt, Bound ty, Bound (tm ty pt), TripleConstraint1 Traversable ty pt tm, Traversable (pt (Pattern pt)), Bitransversable pt) => Pattern pt a -> Term ty pt tm a -> Alt ty pt (Ast ty pt tm) (AstVar a)
tmAlt p tm = Alt (review _Pattern p) s
  where
    vs = fmap ATmVar . toList $ p
    s = abstract (`elemIndex` vs) . review _Unwrapped $ tm

tmCase :: (AsTmCase ty pt tm) => Term ty pt tm a -> [Alt ty pt (Ast ty pt tm) (AstVar a)] -> Term ty pt tm a
tmCase tm alts =
  case N.nonEmpty alts of
    Nothing -> tm
    Just xs -> review _TmCase (tm, xs)
