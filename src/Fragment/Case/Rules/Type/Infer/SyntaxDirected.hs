{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.Case.Rules.Type.Infer.SyntaxDirected (
    CaseInferTypeContext
  , caseInferTypeRules
  ) where

import Ast.Error.Common
import Rules.Type.Infer.SyntaxDirected

import qualified Fragment.Case.Rules.Type.Infer.Common as C

type CaseInferTypeContext e w s r m ki ty pt tm a =
  (C.CaseInferTypeContext e w s r m m ki ty pt tm a, AsExpectedTypeAllEq e ki ty a)

caseInferTypeRules :: CaseInferTypeContext e w s r m ki ty pt tm a
                   => InferTypeInput e w s r m m ki ty pt tm a
caseInferTypeRules =
  let
    ch = C.CaseHelper id expectTypeAllEq
  in
    C.inferTypeInput ch
