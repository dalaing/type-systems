{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.If.Rules.Type.Infer.Common (
    IfHelper(..)
  , inferTypeInput
  ) where

import Control.Lens (review, preview)

import Ast.Type
import Ast.Error.Common
import Ast.Term

import Fragment.Bool.Ast.Type
import Fragment.If.Ast.Term

import Rules.Type.Infer.Common

data IfHelper m ki ty a =
  IfHelper {
    ihExpectType :: ExpectedType ki ty a -> ActualType ki ty a -> m ()
  , ihExpectTypeEq :: Type ki ty a -> Type ki ty a -> m ()
  }

inferTypeInput :: (AsTyBool ki ty, AsTmIf ki ty pt tm, Monad mi)
               => IfHelper mi ki ty a
               -> InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput ih =
  InferTypeInput [] [ InferTypeRecurse $ inferTmIf ih ] []

inferTmIf :: (AsTyBool ki ty, AsTmIf ki ty pt tm, Monad m)
          => IfHelper m ki ty a
          -> (Term ki ty pt tm a -> m (Type ki ty a))
          -> Term ki ty pt tm a
          -> Maybe (m (Type ki ty a))
inferTmIf (IfHelper expectType expectTypeEq) inferFn tm = do
  (tmB, tmT, tmF) <- preview _TmIf tm
  return $ do
    tyB <- inferFn tmB
    expectType (ExpectedType tyB) (ActualType (review _TyBool ()))
    tyT <- inferFn tmT
    tyF <- inferFn tmF
    expectTypeEq tyT tyF
    return tyT

