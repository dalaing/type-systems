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
module Fragment.TmApp.Rules (
    RTmApp
  ) where

import GHC.Exts (Constraint)

import Ast
import Context.Type.Error
import Rules

import Fragment.TmApp.Ast
import qualified Fragment.TmApp.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.TmApp.Rules.Type.Infer.Offline as TUO
import Fragment.TmApp.Rules.Term

data RTmApp

instance AstIn RTmApp where
  type KindList RTmApp = '[]
  type TypeList RTmApp = '[]
  type PatternList RTmApp = '[]
  type TermList RTmApp = '[TmFApp]

instance RulesIn RTmApp where
  type InferKindContextSyntax e w s r m ki ty a RTmApp = (() :: Constraint)
  type InferTypeContextSyntax e w s r m ki ty pt tm a RTmApp = TSD.TmAppInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RTmApp = TUO.TmAppInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RTmApp = (() :: Constraint)
  type RuleTermContext ki ty pt tm a RTmApp = TmAppTermContext ki ty pt tm a
  type ErrorList ki ty pt tm a RTmApp = '[ErrUnboundTypeVariable a]
  type WarningList ki ty pt tm a RTmApp = '[]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = TSD.tmAppInferTypeRules
  inferTypeInputOffline _ = TUO.tmAppInferTypeRules
  typeInput _ = mempty
  termInput _ = tmAppTermRules
