{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Tuple.Rules.Kind.Infer.SyntaxDirected (
    TupleInferKindContext
  , tupleInferKindRules
  ) where

import Data.Foldable (traverse_)

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)
import Data.Functor.Classes (Eq1)

import Ast.Kind
import Ast.Type
import Ast.Error.Common
import Rules.Kind.Infer.SyntaxDirected

import Fragment.KiBase.Ast.Kind
import Fragment.Tuple.Ast.Type

inferTyTuple :: (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyTuple ki ty)
            => (Type ki ty a -> m (Kind ki))
            -> Type ki ty a
            -> Maybe (m (Kind ki))
inferTyTuple inferFn ty = do
  tys <- preview _TyTuple ty
  return $ do
    let ki = review _KiBase()
    traverse_ (\tyT -> mkCheckKind inferFn tyT ki) tys
    return . review _KiBase $ ()

type TupleInferKindContext e w s r m ki ty a = (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyTuple ki ty)

tupleInferKindRules :: TupleInferKindContext e w s r m ki ty a
                    => InferKindInput e w s r m ki ty a
tupleInferKindRules =
  InferKindInput
    [InferKindRecurse inferTyTuple]
