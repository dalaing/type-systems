{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.If.Rules.Infer (
    IfInferContext
  , ifInferRules
  ) where

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Infer
import Ast.Type
import Ast.Term
import Ast.Error.Common
import Data.Functor.Rec

import Fragment.Bool.Ast.Type
import Fragment.If.Ast.Term

inferTmIf :: (Eq a, EqRec ty, MonadError e m, AsUnexpected e ty a, AsExpectedEq e ty a, AsTyBool ty, AsTmIf ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmIf inferFn tm = do
  (tmB, tmT, tmF) <- preview _TmIf tm
  return $ do
    tyB <- inferFn tmB
    expect (ExpectedType tyB) (ActualType (review _TyBool ()))
    tyT <- inferFn tmT
    tyF <- inferFn tmF
    expectEq tyT tyF
    return tyT

type IfInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, AsUnexpected e ty a, AsExpectedEq e ty a, AsTyBool ty, AsTmIf ty pt tm)

ifInferRules :: IfInferContext e w s r m ty pt tm a
             => InferInput e w s r m ty pt tm a
ifInferRules =
  InferInput [ InferRecurse inferTmIf ] []
