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
{-# LANGUAGE DataKinds #-}
module Language.TheLot (
    runEvalStrict
  , runEvalLazy
  , runInfer
  , runCheck
  ) where

-- TODO reexport the helpers
-- TODO probably should put them into their own module for that

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T

import Control.Lens

import Bound
import Data.Functor.Classes
import Data.Deriving

import Fragment
import Fragment.Ast
import Error
import Util

import Fragment.Var
import Fragment.Int
import Fragment.Bool
import Fragment.If
import Fragment.Pair
import Fragment.Tuple
import Fragment.Record
import Fragment.Variant
-- import Fragment.STLC
import Fragment.SystemF
import Fragment.Case

type TypeF = TSum '[TyFInt, TyFBool, TyFPair, TyFTuple, TyFRecord, TyFVariant, TyFSystemF]
type PatternF = TSum '[PtFWild, PtFInt, PtFBool, PtFPair, PtFTuple, PtFRecord, PtFVariant]

data TermF ty pt f a =
    TmLInt (TmFInt ty pt f a)
  | TmLBool (TmFBool ty pt f a)
  | TmLIf (TmFIf ty pt f a)
  | TmLPair (TmFPair ty pt f a)
  | TmLTuple (TmFTuple ty pt f a)
  | TmLRecord (TmFRecord ty pt f a)
  | TmLVariant (TmFVariant ty pt f a)
  | TmLCase (TmFCase ty pt f a)
  -- | TmLSTLC (TmFSTLC ty pt f a)
  | TmLSystemF (TmFSystemF ty pt f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TermF

instance (Eq1 f, EqRec ty, Monad f) => Eq1 (TermF ty pt f) where
  liftEq = $(makeLiftEq ''TermF)

instance (Ord1 f, OrdRec ty, Monad f) => Ord1 (TermF ty pt f) where
  liftCompare = $(makeLiftCompare ''TermF)

instance (Show1 f, ShowRec ty) => Show1 (TermF ty pt f) where
  liftShowsPrec = $(makeLiftShowsPrec ''TermF)

instance AsTmInt ty pt TermF where
  _TmIntP = _TmLInt

instance AsTmBool ty pt TermF where
  _TmBoolP = _TmLBool

instance AsTmIf ty pt TermF where
  _TmIfP = _TmLIf

instance AsTmPair ty pt TermF where
  _TmPairP = _TmLPair

instance AsTmTuple ty pt TermF where
  _TmTupleP = _TmLTuple

instance AsTmRecord ty pt TermF where
  _TmRecordP = _TmLRecord

instance (Bitransversable ty, Bitransversable pt) => AsTmVariant ty pt TermF where
  _TmVariantP = _TmLVariant

instance AsTmCase ty pt TermF where
  _TmCaseP = _TmLCase

--instance (Bitransversable ty, Bitransversable pt) => AsTmSTLC ty pt TermF where
--  _TmSTLCP = _TmLSTLC

instance (Bitransversable ty, Bitransversable pt) => AsTmSystemF ty pt TermF where
  _TmSystemFP = _TmLSystemF

instance EqRec (TermF ty pt) where
  liftEqRec eR e (TmLInt x) (TmLInt y) = liftEqRec eR e x y
  liftEqRec eR e (TmLBool x) (TmLBool y) = liftEqRec eR e x y
  liftEqRec eR e (TmLIf x) (TmLIf y) = liftEqRec eR e x y
  liftEqRec eR e (TmLPair x) (TmLPair y) = liftEqRec eR e x y
  liftEqRec eR e (TmLTuple x) (TmLTuple y) = liftEqRec eR e x y
  liftEqRec eR e (TmLRecord x) (TmLRecord y) = liftEqRec eR e x y
  liftEqRec eR e (TmLVariant x) (TmLVariant y) = liftEqRec eR e x y
  liftEqRec eR e (TmLCase x) (TmLCase y) = liftEqRec eR e x y
  -- liftEqRec eR e (TmLSTLC x) (TmLSTLC y) = liftEqRec eR e x y
  liftEqRec eR e (TmLSystemF x) (TmLSystemF y) = liftEqRec eR e x y
  liftEqRec _ _ _ _ = False

instance OrdRec (TermF ty pt) where
  liftCompareRec cR c (TmLInt x) (TmLInt y) = liftCompareRec cR c x y
  liftCompareRec _ _ (TmLInt _) _ = LT
  liftCompareRec _ _ _ (TmLInt _) = GT
  liftCompareRec cR c (TmLBool x) (TmLBool y) = liftCompareRec cR c x y
  liftCompareRec _ _ (TmLBool _) _ = LT
  liftCompareRec _ _ _ (TmLBool _) = GT
  liftCompareRec cR c (TmLIf x) (TmLIf y) = liftCompareRec cR c x y
  liftCompareRec _ _ (TmLIf _) _ = LT
  liftCompareRec _ _ _ (TmLIf _) = GT
  liftCompareRec cR c (TmLPair x) (TmLPair y) = liftCompareRec cR c x y
  liftCompareRec _ _ (TmLPair _) _ = LT
  liftCompareRec _ _ _ (TmLPair _) = GT
  liftCompareRec cR c (TmLTuple x) (TmLTuple y) = liftCompareRec cR c x y
  liftCompareRec _ _ (TmLTuple _) _ = LT
  liftCompareRec _ _ _ (TmLTuple _) = GT
  liftCompareRec cR c (TmLRecord x) (TmLRecord y) = liftCompareRec cR c x y
  liftCompareRec _ _  (TmLRecord _) _ = LT
  liftCompareRec _ _ _ (TmLRecord _) = GT
  liftCompareRec cR c (TmLVariant x) (TmLVariant y) = liftCompareRec cR c x y
  liftCompareRec _ _  (TmLVariant _) _ = LT
  liftCompareRec _ _ _ (TmLVariant _) = GT
  liftCompareRec cR c (TmLCase x) (TmLCase y) = liftCompareRec cR c x y
  liftCompareRec _ _  (TmLCase _) _ = LT
  liftCompareRec _ _ _ (TmLCase _) = GT
  -- liftCompareRec cR c (TmLSTLC x) (TmLSTLC y) = liftCompareRec cR c x y
  -- liftCompareRec _ _  (TmLSTLC _) _ = LT
  -- liftCompareRec _ _ _ (TmLSTLC _) = GT
  liftCompareRec cR c (TmLSystemF x) (TmLSystemF y) = liftCompareRec cR c x y
  -- liftCompareRec _ _  (TmLSystemF _) _ = LT
  -- liftCompareRec _ _ _ (TmLSystemF _) = GT

instance ShowRec (TermF ty pt) where
  liftShowsPrecRec sR slR s sl n (TmLInt x) = liftShowsPrecRec sR slR s sl n x
  liftShowsPrecRec sR slR s sl n (TmLBool x) = liftShowsPrecRec sR slR s sl n x
  liftShowsPrecRec sR slR s sl n (TmLIf x) = liftShowsPrecRec sR slR s sl n x
  liftShowsPrecRec sR slR s sl n (TmLPair x) = liftShowsPrecRec sR slR s sl n x
  liftShowsPrecRec sR slR s sl n (TmLTuple x) = liftShowsPrecRec sR slR s sl n x
  liftShowsPrecRec sR slR s sl n (TmLRecord x) = liftShowsPrecRec sR slR s sl n x
  liftShowsPrecRec sR slR s sl n (TmLVariant x) = liftShowsPrecRec sR slR s sl n x
  liftShowsPrecRec sR slR s sl n (TmLCase x) = liftShowsPrecRec sR slR s sl n x
  -- liftShowsPrecRec sR slR s sl n (TmLSTLC x) = liftShowsPrecRec sR slR s sl n x
  liftShowsPrecRec sR slR s sl n (TmLSystemF x) = liftShowsPrecRec sR slR s sl n x

instance Bound (TermF ty pt) where
  TmLInt i >>>= f = TmLInt (i >>>= f)
  TmLBool b >>>= f = TmLBool (b >>>= f)
  TmLIf i >>>= f = TmLIf (i >>>= f)
  TmLPair p >>>= f = TmLPair (p >>>= f)
  TmLTuple t >>>= f = TmLTuple (t >>>= f)
  TmLRecord r >>>= f = TmLRecord (r >>>= f)
  TmLVariant v >>>= f = TmLVariant (v >>>= f)
  TmLCase c >>>= f = TmLCase (c >>>= f)
  -- TmLSTLC lc >>>= f = TmLSTLC (lc >>>= f)
  TmLSystemF sf >>>= f = TmLSystemF (sf >>>= f)

instance Bitransversable (TermF ty tp) where
  bitransverse fT fL (TmLInt i) = TmLInt <$> bitransverse fT fL i
  bitransverse fT fL (TmLBool b) = TmLBool <$> bitransverse fT fL b
  bitransverse fT fL (TmLIf i) = TmLIf <$> bitransverse fT fL i
  bitransverse fT fL (TmLPair p) = TmLPair <$> bitransverse fT fL p
  bitransverse fT fL (TmLTuple t) = TmLTuple <$> bitransverse fT fL t
  bitransverse fT fL (TmLRecord r) = TmLRecord <$> bitransverse fT fL r
  bitransverse fT fL (TmLVariant v) = TmLVariant <$> bitransverse fT fL v
  bitransverse fT fL (TmLCase c) = TmLCase <$> bitransverse fT fL c
  -- bitransverse fT fL (TmLSTLC lc) = TmLSTLC <$> bitransverse fT fL lc
  bitransverse fT fL (TmLSystemF sf) = TmLSystemF <$> bitransverse fT fL sf

data Error ty pt tm a =
    EUnexpected (Expected ty a) (Actual ty a)
  | EExpectedEq (Type ty a) (Type ty a)
  | EExpectedTyPair (Type ty a)
  | EExpectedTyTuple (Type ty a)
  | ETupleOutOfBounds Int Int
  | EExpectedTyRecord (Type ty a)
  | ERecordNotFound T.Text
  | EExpectedTyVariant (Type ty a)
  | EVariantNotFound T.Text
  | EExpectedAllEq (N.NonEmpty (Type ty a))
  | EExpectedTyArr (Type ty a)
  | EExpectedTyAll (Type ty a)
  | EUnboundTermVariable a
  | EExpectedPattern (Ast ty pt tm (AstVar a))
  | EDuplicatedPatternVariables (N.NonEmpty a)
  | EUnusedPatternVariables (N.NonEmpty a)
  | EUnknownTypeError

deriving instance (Eq a, AstEq ty pt tm) => Eq (Error ty pt tm a)
deriving instance (Ord a, AstOrd ty pt tm) => Ord (Error ty pt tm a)
deriving instance (Show a, AstShow ty pt tm) => Show (Error ty pt tm a)

makePrisms ''Error

instance AsUnexpected (Error ty pt tm a) ty a where
  _Unexpected = _EUnexpected

instance AsExpectedEq (Error ty pt tm a) ty a where
  _ExpectedEq = _EExpectedEq

instance AsExpectedTyPair (Error ty pt tm a) ty a where
  _ExpectedTyPair = _EExpectedTyPair

instance AsExpectedTyTuple (Error ty pt tm a) ty a where
  _ExpectedTyTuple = _EExpectedTyTuple

instance AsTupleOutOfBounds (Error ty pt tm a) where
  _TupleOutOfBounds = _ETupleOutOfBounds

instance AsExpectedTyRecord (Error ty pt tm a) ty a where
  _ExpectedTyRecord = _EExpectedTyRecord

instance AsRecordNotFound (Error ty pt tm a) where
  _RecordNotFound = _ERecordNotFound

instance AsExpectedTyVariant (Error ty pt tm a) ty a where
  _ExpectedTyVariant = _EExpectedTyVariant

instance AsVariantNotFound (Error ty pt tm a) where
  _VariantNotFound = _EVariantNotFound

instance AsExpectedAllEq (Error ty pt tm a) ty a where
  _ExpectedAllEq = _EExpectedAllEq

instance AsExpectedTyArr (Error ty pt tm a) ty a where
  _ExpectedTyArr = _EExpectedTyArr

instance AsExpectedTyAll (Error ty pt tm a) ty a where
  _ExpectedTyAll = _EExpectedTyAll

instance AsUnboundTermVariable (Error ty pt tm a) a where
  _UnboundTermVariable = _EUnboundTermVariable

instance AsExpectedPattern (Error ty pt tm a) ty pt tm a where
  _ExpectedPattern = _EExpectedPattern

instance AsDuplicatedPatternVariables (Error ty pt tm a) a where
  _DuplicatedPatternVariables = _EDuplicatedPatternVariables

instance AsUnusedPatternVariables (Error ty pt tm a) a where
  _UnusedPatternVariables = _EUnusedPatternVariables

instance AsUnknownTypeError (Error ty pt tm a) where
  _UnknownTypeError = _EUnknownTypeError

type LContext e s r m ty pt tm a =
  ( TmVarContext e s r m ty pt tm a
  , PtVarContext e s r m ty pt tm a
  , IntContext e s r m ty pt tm a
  , BoolContext e s r m ty pt tm a
  , IfContext e s r m ty pt tm a
  , PairContext e s r m ty pt tm a
  , TupleContext e s r m ty pt tm a
  , RecordContext e s r m ty pt tm a
  , VariantContext e s r m ty pt tm a
  , CaseContext e s r m ty pt tm a
  -- , STLCContext e s r m ty pt tm a
  , SystemFContext e s r m ty pt tm a
  , AsUnknownTypeError e
  )

fragmentInputBase :: LContext e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
fragmentInputBase = mconcat [ptVarFragment, tmVarFragment, intFragment, boolFragment, ifFragment]

fragmentInputLazy :: LContext e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
fragmentInputLazy =
  mconcat
    [ fragmentInputBase
    , pairFragmentLazy
    , tupleFragmentLazy
    , recordFragmentLazy
    , variantFragmentLazy
    , caseFragmentLazy
    -- , stlcFragmentLazy
    , systemFFragmentLazy
    ]

fragmentInputStrict :: LContext e s r m ty pt tm a => FragmentInput e s r m ty pt tm a
fragmentInputStrict =
  mconcat
    [ fragmentInputBase
    , pairFragmentStrict
    , tupleFragmentStrict
    , recordFragmentStrict
    , variantFragmentStrict
    , caseFragmentStrict
    -- , stlcFragmentStrict
    , systemFFragmentStrict
    ]

type M e s r = StateT s (ReaderT r (Except e))

runM :: s -> r -> M e s r a -> Either e a
runM s r m =
  runExcept .
  flip runReaderT r .
  flip evalStateT s $
  m

type LTerm = Term TypeF PatternF TermF String
type LType = Type TypeF String
type LError = Error TypeF PatternF TermF String

-- TODO could use different supplies for the variables

type Output = FragmentOutput LError Int (TermContext TypeF String String) (M LError Int (TermContext TypeF String String)) TypeF PatternF TermF String

fragmentOutputLazy :: Output
fragmentOutputLazy = prepareFragmentLazy fragmentInputLazy

fragmentOutputStrict :: Output
fragmentOutputStrict = prepareFragmentStrict fragmentInputStrict

runEvalLazy :: LTerm -> LTerm
runEvalLazy =
  foEval fragmentOutputLazy

runEvalStrict :: LTerm  -> LTerm
runEvalStrict =
  foEval fragmentOutputStrict

runInfer :: LTerm -> Either LError LType
runInfer =
  runM 0 emptyTermContext .
  foInfer fragmentOutputLazy

runCheck :: LTerm -> LType -> Either LError ()
runCheck tm ty =
  runM 0 emptyTermContext $ foCheck fragmentOutputLazy tm ty

-- for debugging

runStepLazy :: LTerm -> Maybe LTerm
runStepLazy =
  foStep fragmentOutputLazy

runValueStrict :: LTerm -> Maybe LTerm
runValueStrict =
  foValue fragmentOutputStrict

runStepStrict :: LTerm -> Maybe LTerm
runStepStrict =
  foStep fragmentOutputStrict
