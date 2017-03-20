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
module Fragment.TmVar.Rules (
    RTmVar
  ) where

import GHC.Exts (Constraint)

import Ast
import Context.Term.Error
import Rules

import qualified Fragment.TmVar.Rules.Type.Infer.SyntaxDirected as SD
import qualified Fragment.TmVar.Rules.Type.Infer.Offline as UO

data RTmVar

instance AstIn RTmVar where
  type KindList RTmVar = '[]
  type TypeList RTmVar = '[]
  type PatternList RTmVar = '[]
  type TermList RTmVar = '[]

instance RulesIn RTmVar where
  type InferKindContextSyntax e w s r m ki ty a RTmVar = (() :: Constraint)
  type InferTypeContextSyntax e w s r m ki ty pt tm a RTmVar = SD.TmVarInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RTmVar = UO.TmVarInferTypeContext e w s r m ki ty pt tm a
  type ErrorList ki ty tm pt a RTmVar = '[ErrUnboundTermVariable a]
  type WarningList ki ty tm pt a RTmVar = '[]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = SD.tmVarInferTypeRules
  inferTypeInputOffline _ = UO.tmVarInferTypeRules
