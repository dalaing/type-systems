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

import Ast
-- import Rules
import Rules.Unification
-- import qualified Rules.Type.Infer.SyntaxDirected as SD
-- import qualified Rules.Type.Infer.Offline as UO
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Type.Infer.SyntaxDirected (ISyntax)
import Rules.Type.Infer.Offline (IOffline)
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

import Fragment.SystemF
-- import Fragment.SystemFw
import Fragment.TyArr
import Fragment.TmLam
import Fragment.TmApp
import Fragment.LC

type Rules =
  '[ PtVarTag
   , PtWildTag
   , TmVarTag
   , TyVarTag
   , IntTag
   , BoolTag
   -- , IfTag
   -- , PairTag
   -- , TupleTag
   , RecordTag
   , VariantTag
   -- , SystemFTag
   -- , CaseTag
   -- , FixTag
   -- , TyArrTag
   -- , TmLamTag
   -- , TmAppTag
   -- , LCTag
   ]

rules :: Proxy Rules
rules = Proxy

type KindF = RKindF Rules
type TypeF = RTypeF Rules
type PatternF = RPatternF Rules
type TermF = RTermF Rules

type LTerm = Term KindF TypeF PatternF TermF String
type LType = Type KindF TypeF String
type LError i = RError KindF TypeF PatternF TermF String i Rules
type LWarning i = RWarning KindF TypeF PatternF TermF String i Rules

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

eLazy :: Proxy ELazy
eLazy = Proxy

eStrict :: Proxy EStrict
eStrict = Proxy

runEvalLazy :: LTerm -> LTerm
runEvalLazy =
  eoEval . evalOutput eLazy $ rules

runEvalStrict :: LTerm  -> LTerm
runEvalStrict =
  eoEval . evalOutput eStrict $ rules

normalize :: LType -> LType
normalize =
  noNormalizeType . normalizeOutput $ rules

monadProxy :: Proxy i -> Proxy (MonadProxy (LError i) (LWarning i) Int LContext (M (LError i) (LWarning i) Int LContext))
monadProxy _ = Proxy

syntaxProxy :: Proxy ISyntax
syntaxProxy = Proxy

offlineProxy :: Proxy IOffline
offlineProxy = Proxy

--runInfer :: Proxy i -> LTerm -> (Either (LError i) LType, [LWarning i])
--runInfer i =
--  runM (0 :: Int) emptyContext .
--  ioInfer (inferTypeOutput (monadProxy i) i rules' undefined normalize)

runInferSyntax :: LTerm -> (Either (LError ISyntax) LType, [LWarning ISyntax])
runInferSyntax =
  runM (0 :: Int) emptyContext .
  ioInfer (inferTypeOutput (monadProxy syntaxProxy) syntaxProxy rules undefined normalize)

runCheckSyntax :: LTerm -> LType -> (Either (LError ISyntax) (), [LWarning ISyntax])
runCheckSyntax tm ty =
  runM (0 :: Int) emptyContext $
  ioCheck (inferTypeOutput (monadProxy syntaxProxy) syntaxProxy rules undefined normalize) tm ty

runInferOffline :: LTerm -> (Either (LError IOffline) LType, [LWarning IOffline])
runInferOffline =
  runM (0 :: Int) emptyContext .
  ioInfer (inferTypeOutput (monadProxy offlineProxy) offlineProxy rules undefined normalize)

runCheckOffline :: LTerm -> LType -> (Either (LError IOffline) (), [LWarning IOffline])
runCheckOffline tm ty =
  runM (0 :: Int) emptyContext $
  ioCheck (inferTypeOutput (monadProxy offlineProxy) offlineProxy rules undefined normalize) tm ty

{-
runUnify :: [UConstraint (Type KindF TypeF) String] -> (Either LError (M.Map String (LType)), [LWarning])
runUnify =
  runM (0 :: Int) emptyContext .
  UO.ioUnify (inferTypeOutputOffline rules normalize :: UO.InferTypeOutput LError LWarning Int LContext (M LError LWarning Int LContext) KindF TypeF PatternF TermF String)
-}

-- for debugging

runStepStrict :: LTerm -> Maybe LTerm
runStepStrict =
  eoStep . evalOutput eStrict $ rules

runStepLazy :: LTerm -> Maybe LTerm
runStepLazy =
  eoStep . evalOutput eLazy $ rules

runValueStrict :: LTerm -> Maybe LTerm
runValueStrict =
  eoValue . evalOutput eStrict $ rules

runValueLazy :: LTerm -> Maybe LTerm
runValueLazy =
  eoValue . evalOutput eLazy $ rules

