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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.TheLot (
    runEvalStrict
  , runEvalLazy
  , runInfer
  , runCheck
  ) where

-- TODO reexport the helpers
-- TODO probably should put them into their own module for that

import Control.Monad (ap)
import Data.Void

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Fragment
import Error

-- import Fragment.AST
import Fragment.Var
import Fragment.Int
import Fragment.Bool
import Fragment.Pair
import Fragment.Tuple
import Fragment.Record
-- import Fragment.Variant
-- import Fragment.STLC

data Type a =
    TyLInt (TyFInt Type a)
  | TyLBool (TyFBool Type a)
  | TyLPair (TyFPair Type a)
  | TyLTuple (TyFTuple Type a)
  | TyLRecord (TyFRecord Type a)
--  | TyLVariant (TyFVariant Type a)
--  | TyLSTLC (TyFSTLC Type a)
  deriving (Functor, Foldable, Traversable)

deriveEq1 ''Type
deriveOrd1 ''Type
deriveShow1 ''Type

instance Eq a => Eq (Type a) where (==) = eq1
instance Ord a => Ord (Type a) where compare = compare1
instance Show a => Show (Type a) where showsPrec = showsPrec1

makePrisms ''Type

instance AsTyInt Type where
  _TyIntP = _TyLInt

instance AsTyBool Type where
  _TyBoolP = _TyLBool

instance AsTyPair Type where
  _TyPairP = _TyLPair

instance AsTyTuple Type where
  _TyTupleP = _TyLTuple

instance AsTyRecord Type where
  _TyRecordP = _TyLRecord

-- instance AsTyVariant Type where
--  _TyVariantP = _TyLVariant

--instance AsTySTLC Type where
--  _TySTLCP = _TyLSTLC

data Pattern a =
    PtLVar (PtFVar Pattern a)
  | PtLInt (PtFInt Pattern a)
  | PtLBool (PtFBool Pattern a)
  | PtLPair (PtFPair Pattern a)
  | PtLTuple (PtFTuple Pattern a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''Pattern

instance AsPtVar Pattern where
  _PtVarP = _PtLVar

instance AsPtInt Pattern where
  _PtIntP = _PtLInt

instance AsPtBool Pattern where
  _PtBoolP = _PtLBool

instance AsPtPair Pattern where
  _PtPairP = _PtLPair

instance AsPtTuple Pattern where
  _PtTupleP = _PtLTuple

data Term a =
    TmLVar a
  | TmLInt (TmFInt Term a)
  | TmLBool (TmFBool Term a)
  | TmLPair (TmFPair Term a)
  | TmLTuple (TmFTuple Term a)
  | TmLRecord (TmFRecord Term a)
--  | TmLVariant (TmFVariant Type Void Term a)
--  | TmLSTLC (TmFSTLC Type Void Term a)
  deriving (Functor, Foldable, Traversable)

deriveEq1 ''Term
deriveOrd1 ''Term
deriveShow1 ''Term

instance Eq a => Eq (Term a) where (==) = eq1
instance Ord a => Ord (Term a) where compare = compare1
instance Show a => Show (Term a) where showsPrec = showsPrec1

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = TmLVar

  TmLVar x >>= f = f x
  TmLInt i >>= f = TmLInt (i >>>= f)
  TmLBool b >>= f = TmLBool (b >>>= f)
  TmLPair p >>= f = TmLPair (p >>>= f)
  TmLTuple t >>= f = TmLTuple (t >>>= f)
  TmLRecord r >>= f = TmLRecord (r >>>= f)
--  TmLVariant v >>= f = TmLVariant (v >>>= f)
--  TmLSTLC s >>= f = TmLSTLC (s >>>= f)

makePrisms ''Term

instance AsTmVar Term where
  _TmVar = _TmLVar

instance AsTmInt Term where
  _TmIntP = _TmLInt

instance AsTmBool Term where
  _TmBoolP = _TmLBool

instance AsTmPair Term where
  _TmPairP = _TmLPair

instance AsTmTuple Term where
  _TmTupleP = _TmLTuple

instance AsTmRecord Term where
  _TmRecordP = _TmLRecord

-- instance AsTmVariant Type Term where
--  _TmVariantP = _TmLVariant

-- instance AsTmSTLC Type Term where
--  _TmSTLCP = _TmLSTLC

data Error ty a =
    EUnexpected (ty a) (ty a)
  | EExpectedEq (ty a) (ty a)
  | EExpectedTyPair (ty a)
  | EExpectedTyTuple (ty a)
  | ETupleOutOfBounds Int Int
  | EExpectedTyRecord (ty a)
  | ERecordNotFound T.Text
  | EExpectedTyVariant (ty a)
  | EVariantNotFound T.Text
  | EExpectedAllEq (N.NonEmpty (ty a))
--  | EExpectedTyArr (ty a)
  | EUnboundTermVariable a
  | EUnknownTypeError
  deriving (Eq, Ord, Show)

makePrisms ''Error

instance AsUnexpected (Error ty a) (ty a) where
  _Unexpected = _EUnexpected

instance AsExpectedEq (Error ty a) (ty a) where
  _ExpectedEq = _EExpectedEq

instance AsExpectedTyPair (Error ty a) (ty a) where
  _ExpectedTyPair = _EExpectedTyPair

instance AsExpectedTyTuple (Error ty a) (ty a) where
  _ExpectedTyTuple = _EExpectedTyTuple

instance AsTupleOutOfBounds (Error ty a) where
  _TupleOutOfBounds = _ETupleOutOfBounds

instance AsExpectedTyRecord (Error ty a) (ty a) where
  _ExpectedTyRecord = _EExpectedTyRecord

instance AsRecordNotFound (Error ty a) where
  _RecordNotFound = _ERecordNotFound

-- instance AsExpectedTyVariant (Error ty a) (ty a) where
--  _ExpectedTyVariant = _EExpectedTyVariant

-- instance AsVariantNotFound (Error ty a) where
--  _VariantNotFound = _EVariantNotFound

-- instance AsExpectedAllEq (Error ty a) (ty a) where
--  _ExpectedAllEq = _EExpectedAllEq

-- instance AsExpectedTyArr (Error ty a) (ty a) where
--  _ExpectedTyArr = _EExpectedTyArr

instance AsUnboundTermVariable (Error ty a) a where
  _UnboundTermVariable = _EUnboundTermVariable

instance AsUnknownTypeError (Error ty a) where
  _UnknownTypeError = _EUnknownTypeError

type LContext e s r m ty p tm a =
  ( TmVarContext e s r m ty p tm a
  , PtVarContext e s r m ty p tm a
  , IntContext e s r m ty p tm a
  , BoolContext e s r m ty p tm a
  , PairContext e s r m ty p tm a
  , TupleContext e s r m ty p tm a
  , RecordContext e s r m ty p tm a
  , AsUnknownTypeError e
  )

fragmentInputBase :: LContext e s r m ty p tm a => FragmentInput e s r m ty p tm a
fragmentInputBase = mconcat [ptVarFragment, tmVarFragment, intFragment, boolFragment]

fragmentInputLazy :: LContext e s r m ty p tm a => FragmentInput e s r m ty p tm a
fragmentInputLazy = mconcat [fragmentInputBase, pairFragmentLazy, tupleFragmentLazy, recordFragmentLazy]

fragmentInputStrict :: LContext e s r m ty p tm a => FragmentInput e s r m ty p tm a
fragmentInputStrict = mconcat [fragmentInputBase, pairFragmentStrict, tupleFragmentStrict, recordFragmentStrict]

type M e s r = StateT s (ReaderT r (Except e))

runM :: s -> r -> M e s r a -> Either e a
runM s r m =
  runExcept .
  flip runReaderT r .
  flip evalStateT s $
  m

type Output a = FragmentOutput (Error Type a) Int (TermContext Type a a) (M (Error Type a) Int (TermContext Type a a)) Type Pattern Term a

fragmentOutputLazy :: (Ord a, Eq (Type a), ToTmVar a) => Output a
fragmentOutputLazy = prepareFragment fragmentInputLazy

fragmentOutputStrict :: (Ord a, Eq (Type a), ToTmVar a) => Output a
fragmentOutputStrict = prepareFragment fragmentInputStrict

runEvalLazy :: (Ord a, Eq (Type a), ToTmVar a) => Term a -> Term a
runEvalLazy =
  foEval fragmentOutputLazy

runEvalStrict :: (Ord a, Eq (Type a), ToTmVar a) => Term a -> Term a
runEvalStrict =
  foEval fragmentOutputStrict

runInfer :: (Ord a, ToTmVar a) => Term a -> Either (Error Type a) (Type a)
runInfer =
  runM 0 emptyTermContext .
  foInfer fragmentOutputLazy

runCheck :: (Ord a, ToTmVar a) => Term a -> Type a -> Either (Error Type a) ()
runCheck tm ty =
  runM 0 emptyTermContext $ foCheck fragmentOutputLazy tm ty
