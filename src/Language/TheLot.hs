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

-- import Util

import Ast.Type
import Ast.Error
import Ast.Error.Common
import Ast.Pattern
import Ast.Term
import Context.Term
import Context.Term.Error

import Rules
import Rules.Infer
import Rules.Eval

import Fragment.PtVar
import Fragment.PtWild
import Fragment.TmVar
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

type TypeF =
  TySum '[ TyFInt
         , TyFBool
         , TyFPair
         , TyFTuple
         , TyFRecord
         , TyFVariant
         , TyFSystemF
         ]

type PatternF =
  PtSum '[ PtFWild
         , PtFInt
         , PtFBool
         , PtFPair
         , PtFTuple
         , PtFRecord
         , PtFVariant
         ]

type TermF =
  TmSum '[ TmFInt
         , TmFBool
         , TmFIf
         , TmFPair
         , TmFTuple
         , TmFRecord
         , TmFVariant
         , TmFSystemF
         ]

type Error ty pt tm a =
  ErrSum '[ ErrUnexpected ty a
          , ErrExpectedEq ty a
          , ErrExpectedAllEq ty a
          , ErrExpectedTyPair ty a
          , ErrExpectedTyTuple ty a
          , ErrTupleOutOfBounds
          , ErrExpectedTyRecord ty a
          , ErrRecordNotFound
          , ErrExpectedTyVariant ty a
          , ErrVariantNotFound,
            ErrExpectedTyArr ty a
          , ErrExpectedTyAll ty a
          , ErrUnboundTermVariable a
          , ErrExpectedPattern ty pt tm a
          , ErrDuplicatedPatternVariables a
          , ErrUnusedPatternVariables a
          , ErrUnknownTypeError
          ] ty pt tm a

type LTerm = Term TypeF PatternF TermF String
type LType = Type TypeF String
type LError = Error TypeF PatternF TermF String

type LContext e s r m ty pt tm a =
  ( PtVarContext e s r m ty pt tm a
  , PtWildContext e s r m ty pt tm a
  , TmVarContext e s r m ty pt tm a
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
  )

rulesInput:: LContext e s r m ty pt tm a => RulesInput e s r m ty pt tm a
rulesInput =
  mconcat
    [ ptVarRules
    , ptWildRules
    , tmVarRules
    , intRules
    , boolRules
    , ifRules
    , pairRules
    , tupleRules
    , recordRules
    , variantRules
    , caseRules
    -- , stlcRules
    , systemFRules
    ]

type M e s r = StateT s (ReaderT r (Except e))

runM :: s -> r -> M e s r a -> Either e a
runM s r m =
  runExcept .
  flip runReaderT r .
  flip evalStateT s $
  m

-- TODO could use different supplies for the variables

type Output = RulesOutput LError Int (TermContext TypeF String) (M LError Int (TermContext TypeF String)) TypeF PatternF TermF String

rulesOutput :: Output
rulesOutput = prepareRules rulesInput

runEvalLazy :: LTerm -> LTerm
runEvalLazy =
  eoEval . roEvalOutputLazy $ rulesOutput

runEvalStrict :: LTerm  -> LTerm
runEvalStrict =
  eoEval . roEvalOutputStrict $ rulesOutput

runInfer :: LTerm -> Either LError LType
runInfer =
  runM 0 emptyTermContext .
  (ioInfer . roInferOutput $ rulesOutput)

runCheck :: LTerm -> LType -> Either LError ()
runCheck tm ty =
  runM 0 emptyTermContext $ (ioCheck . roInferOutput $ rulesOutput) tm ty

-- for debugging

runStepLazy :: LTerm -> Maybe LTerm
runStepLazy =
  (eoStep . roEvalOutputLazy) rulesOutput

runValueStrict :: LTerm -> Maybe LTerm
runValueStrict =
  (eoValue . roEvalOutputStrict) rulesOutput

runStepStrict :: LTerm -> Maybe LTerm
runStepStrict =
  (eoStep . roEvalOutputStrict) rulesOutput
