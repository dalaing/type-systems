{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TmLam.Rules (
    RTmLam
  ) where

import GHC.Exts (Constraint)

import Ast
import Context.Type.Error
import Rules

import Fragment.TyArr.Ast.Type

import Fragment.TmLam.Ast
import qualified Fragment.TmLam.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.TmLam.Rules.Type.Infer.Offline as TUO

data RTmLam

instance AstIn RTmLam where
  type KindList RTmLam = '[]
  type TypeList RTmLam = '[TyFArr]
  type PatternList RTmLam = '[]
  type TermList RTmLam = '[TmFLam]

instance RulesIn RTmLam where
  type InferKindContextSyntax e w s r m ki ty a RTmLam = (() :: Constraint)
  type InferTypeContextSyntax e w s r m ki ty pt tm a RTmLam = TSD.TmLamInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RTmLam = TUO.TmLamInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RTmLam = (() :: Constraint)
  type ErrorList ki ty pt tm a RTmLam = '[ErrExpectedTmLamAnnotation, ErrUnboundTypeVariable a]
  type WarningList ki ty pt tm a RTmLam = '[]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = TSD.tmLamInferTypeRules
  inferTypeInputOffline _ = TUO.tmLamInferTypeRules
  typeInput _ = mempty
