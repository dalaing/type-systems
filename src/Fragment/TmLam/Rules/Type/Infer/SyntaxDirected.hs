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
module Fragment.TmLam.Rules.Type.Infer.SyntaxDirected (
    TmLamInferTypeContext
  , tmLamInferTypeRules
  ) where

import Bound (Scope)
import Control.Lens (preview)
import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Ast.Type
import Ast.Term
import Rules.Type.Infer.SyntaxDirected

import Fragment.TmLam.Ast.Error
import Fragment.TmLam.Ast.Term

import qualified Fragment.TmLam.Rules.Type.Infer.Common as L

type TmLamInferTypeContext e w s r m ki ty pt tm a =
  ( L.TmLamInferTypeContext e w s r m m ki ty pt tm a
  , MonadError e m
  , AsExpectedTmLamAnnotation e
  )

expectTmLam :: TmLamInferTypeContext e w s r m ki ty pt tm a
            => Term ki ty pt tm a
            -> Maybe (m (Type ki ty a, Scope () (Ast ki ty pt tm) (AstVar a)))
expectTmLam tm = do
  (mty, s) <- preview _TmLam tm
  return $ do
    case mty of
      Nothing -> throwing _ExpectedTmLamAnnotation ()
      Just ty -> return (ty, s)

tmLamInferTypeRules :: TmLamInferTypeContext e w s r m ki ty pt tm a
                    => InferTypeInput e w s r m m ki ty pt tm a
tmLamInferTypeRules =
  let
    lh = L.TmLamHelper expectTmLam
  in
    L.inferTypeInput lh
