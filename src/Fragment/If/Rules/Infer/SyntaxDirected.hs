{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.If.Rules.Infer.SyntaxDirected (
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

inferTmIf :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpected e ki ty a, AsExpectedEq e ki ty a, AsTyBool ki ty, AsTmIf ki ty pt tm) => (Type ki ty a -> Type ki ty a -> Bool) -> (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmIf tyEquiv inferFn tm = do
  (tmB, tmT, tmF) <- preview _TmIf tm
  return $ do
    tyB <- inferFn tmB
    expect tyEquiv (ExpectedType tyB) (ActualType (review _TyBool ()))
    tyT <- inferFn tmT
    tyF <- inferFn tmF
    expectEq tyEquiv tyT tyF
    return tyT

type IfInferContext e w s r m ki ty pt tm a = (InferContext e w s r m ki ty pt tm a, AsUnexpected e ki ty a, AsExpectedEq e ki ty a, AsTyBool ki ty, AsTmIf ki ty pt tm)

ifInferRules :: IfInferContext e w s r m ki ty pt tm a
             => InferInput e w s r m ki ty pt tm a
ifInferRules =
  InferInput [] [ InferTyEquivRecurse inferTmIf ] []
