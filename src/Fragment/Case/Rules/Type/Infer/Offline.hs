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
module Fragment.Case.Rules.Type.Infer.Offline (
    CaseInferTypeContext
  , caseInferTypeRules
  ) where

import Control.Monad.Trans (lift)

import Rules.Type.Infer.Offline

import qualified Fragment.Case.Rules.Type.Infer.Common as C

type CaseInferTypeContext e w s r m ki ty pt tm a = C.CaseInferTypeContext e w s r m (UnifyT ki ty a m) ki ty pt tm a

caseInferTypeRules :: CaseInferTypeContext e w s r m ki ty pt tm a
                   => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
caseInferTypeRules =
  let
    ch = C.CaseHelper lift expectTypeAllEq
  in
    C.inferTypeInput ch
