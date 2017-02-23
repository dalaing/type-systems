{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.TmVar.Rules (
    TmVarContext
  , tmVarRules
  ) where

import Rules

import Fragment.TmVar.Rules.Infer

type TmVarContext e s r m ty pt tm a = TmVarInferContext e s r m ty pt tm a

tmVarRules :: TmVarContext e s r m ty pt tm a
           => RulesInput e s r m ty pt tm a
tmVarRules =
  RulesInput tmVarInferRules mempty mempty
