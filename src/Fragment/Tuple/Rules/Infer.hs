{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Tuple.Rules.Infer (
    TupleInferContext
  , tupleInferRules
  ) where

import Control.Monad (zipWithM)

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Infer
import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Tuple.Ast.Type
import Fragment.Tuple.Ast.Error
import Fragment.Tuple.Ast.Pattern
import Fragment.Tuple.Ast.Term

inferTmTuple :: (Monad m, AsTyTuple ty, AsTmTuple ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmTuple inferFn tm = do
  tms <- preview _TmTuple tm
  return $ do
    tys <- traverse inferFn tms
    return $ review _TyTuple tys

inferTmTupleIx :: (MonadError e m, AsExpectedTyTuple e ty a, AsTupleOutOfBounds e, AsTyTuple ty, AsTmTuple ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmTupleIx inferFn tm = do
  (tmT, i) <- preview _TmTupleIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectTyTuple tyT
    lookupTuple tys i

checkTuple :: (MonadError e m, AsExpectedTyTuple e ty a, AsTyTuple ty, AsPtTuple pt) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkTuple checkFn p ty = do
  pts <- preview _PtTuple p
  return $ do
    tys <- expectTyTuple ty
    ms <- zipWithM checkFn pts tys
    return $ mconcat ms

type TupleInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, AsTyTuple ty, AsExpectedTyTuple e ty a, AsTupleOutOfBounds e, AsPtTuple pt, AsTmTuple ty pt tm)

tupleInferRules :: TupleInferContext e w s r m ty pt tm a
                => InferInput e w s r m ty pt tm a
tupleInferRules =
  InferInput
    [ InferRecurse inferTmTuple
    , InferRecurse inferTmTupleIx
    ]
    [ PCheckRecurse checkTuple ]
