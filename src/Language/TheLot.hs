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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Control.Lens ((&), (.~))
import Control.Lens.TH (makeLenses)

import qualified Data.Map as M

import Ast.Type
import Ast.Term
import qualified Context.Type as CTy
import qualified Context.Term as CTm

import Rules
import Rules.Unification
import qualified Rules.Type.Infer.SyntaxDirected as SD
import qualified Rules.Type.Infer.Offline as UO
import Rules.Term

import Fragment.PtVar
import Fragment.PtWild
import Fragment.TmVar
import Fragment.TyVar
import Fragment.KiBase
import Fragment.Int
import Fragment.Bool
import Fragment.If
import Fragment.Pair
import Fragment.Tuple
import Fragment.Record
import Fragment.Variant
import Fragment.Case
import Fragment.Fix
-- import Fragment.STLC
-- import Fragment.HM
--import Fragment.SystemFw
import Fragment.TyArr
import Fragment.TmLam
import Fragment.TmApp
import Fragment.LC

type Rules =
  '[ RPtVar
   , RPtWild
   , RTmVar
   , RTyVar
   , RInt
   , RBool
   , RIf
   , RPair
   , RTuple
   , RRecord
   , RVariant
   , RCase
   , RFix
   -- , RHM
   -- , RSystemFw
   , RTyArr
   , RTmLam
   , RTmApp
   , RLC
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

data LContext =
  LContext {
    _lTermContext :: CTm.TermContext KindF TypeF String
  , _lTypeContext :: CTy.TypeContext KindF String
  }

makeLenses ''LContext

emptyContext :: LContext
emptyContext = LContext CTm.emptyTermContext CTy.emptyTypeContext

instance CTm.HasTermContext' LContext where
  type TmCtxKi LContext = KindF
  type TmCtxTy LContext = TypeF
  type TmCtxVar LContext = String
  termContext = lTermContext

instance CTy.HasTypeContext' LContext where
  type TyCtxKi LContext = KindF
  type TyCtxVar LContext = String
  typeContext = lTypeContext

runEvalLazy :: LTerm -> LTerm
runEvalLazy =
  eoEval . toEvalLazy . termOutput $ rules

runEvalStrict :: LTerm  -> LTerm
runEvalStrict =
  eoEval . toEvalStrict . termOutput $ rules

runInferSyntax :: LTerm -> (Either LError LType, [LWarning])
runInferSyntax =
  runM (0 :: Int) emptyContext .
  SD.ioInfer (inferTypeOutputSyntax rules)

runCheckSyntax :: LTerm -> LType -> (Either LError (), [LWarning])
runCheckSyntax tm ty =
  runM (0 :: Int) emptyContext $
  (SD.ioCheck $ inferTypeOutputSyntax rules) tm ty

runInferOffline :: LTerm -> (Either LError LType, [LWarning])
runInferOffline =
  runM (0 :: Int) emptyContext .
  UO.ioInfer (inferTypeOutputOffline rules)

runCheckOffline :: LTerm -> LType -> (Either LError (), [LWarning])
runCheckOffline tm ty =
  runM (0 :: Int) emptyContext $
  (UO.ioCheck $ inferTypeOutputOffline rules) tm ty

runUnify :: [UConstraint (Type KindF TypeF) String] -> (Either LError (M.Map String (LType)), [LWarning])
runUnify =
  runM (0 :: Int) emptyContext .
  UO.ioUnify (inferTypeOutputOffline rules :: UO.InferTypeOutput LError LWarning Int LContext (M LError LWarning Int LContext) KindF TypeF PatternF TermF String)

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

