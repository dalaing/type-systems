{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Language.TheLot (
    runEvalStrict
  , runEvalLazy
  , runStepStrict
  , runStepLazy
  , runValueStrict
  , runValueLazy
  , runInferSyntax
  , runCheckSyntax
  , runInferOffline
  , runCheckOffline
  ) where

import Data.Proxy

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State

import Ast.Type
import Ast.Term
import Context.Term

import Rules
import qualified Rules.Infer.SyntaxDirected as SD
import qualified Rules.Infer.Unification.Offline as UO
import Rules.Term

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
import Fragment.HM
-- import Fragment.SystemF
import Fragment.Case

type Rules =
  '[ RPtVar
   , RPtWild
   , RTmVar
   , RInt
   , RBool
   , RIf
   , RPair
   , RTuple
   , RRecord
   , RVariant
   , RCase
   , RHM
   -- , RSystemF
   ]

rules :: Proxy Rules
rules = Proxy

type KindF = RKindF Rules
type TypeF = RTypeF Rules
type PatternF = RPatternF Rules
type TermF = RTermF Rules

type LTerm = Term KindF TypeF PatternF TermF String
type LType = Type KindF TypeF String
type LError = RError KindF TypeF PatternF TermF String Rules
type LWarning = RWarning KindF TypeF PatternF TermF String Rules

type M e w s r = StateT s (ReaderT r (ExceptT e (Writer [w])))

runM :: s -> r -> M e w s r a -> (Either e a, [w])
runM s r m =
  runWriter .
  runExceptT .
  flip runReaderT r .
  flip evalStateT s $
  m

runEvalLazy :: LTerm -> LTerm
runEvalLazy =
  eoEval . toEvalLazy . termOutput $ rules

runEvalStrict :: LTerm  -> LTerm
runEvalStrict =
  eoEval . toEvalStrict . termOutput $ rules

runInferSyntax :: LTerm -> (Either LError LType, [LWarning])
runInferSyntax =
  runM (0 :: Int) emptyTermContext .
  SD.ioInfer (inferSyntaxOutput rules)

runCheckSyntax :: LTerm -> LType -> (Either LError (), [LWarning])
runCheckSyntax tm ty =
  runM (0 :: Int) emptyTermContext $
  (SD.ioCheck $ inferSyntaxOutput rules) tm ty

runInferOffline :: LTerm -> (Either LError LType, [LWarning])
runInferOffline =
  runM (0 :: Int) emptyTermContext .
  UO.ioInfer (inferOfflineOutput rules)

runCheckOffline :: LTerm -> LType -> (Either LError (), [LWarning])
runCheckOffline tm ty =
  runM (0 :: Int) emptyTermContext $
  (UO.ioCheck $ inferOfflineOutput rules) tm ty

-- for debugging

runStepStrict :: LTerm -> Maybe LTerm
runStepStrict =
  eoStep . toEvalStrict . termOutput $ rules

runStepLazy :: LTerm -> Maybe LTerm
runStepLazy =
  eoStep . toEvalLazy . termOutput $ rules

runValueStrict :: LTerm -> Maybe LTerm
runValueStrict =
  eoValue . toEvalStrict . termOutput $ rules

runValueLazy :: LTerm -> Maybe LTerm
runValueLazy =
  eoValue . toEvalLazy . termOutput $ rules

