{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Record.Rules.Kind.Infer.SyntaxDirected (
    RecordInferKindContext
  , recordInferKindRules
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
import Fragment.Record.Ast.Type

inferTyRecord :: (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyRecord ki ty)
            => (Type ki ty a -> m (Kind ki))
            -> Type ki ty a
            -> Maybe (m (Kind ki))
inferTyRecord inferFn ty = do
  tys <- preview _TyRecord ty
  return $ do
    let ki = review _KiBase()
    traverse_ (\(_, tyR) -> mkCheckKind inferFn tyR ki) tys
    return . review _KiBase $ ()

type RecordInferKindContext e w s r m ki ty a = (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyRecord ki ty)

recordInferKindRules :: RecordInferKindContext e w s r m ki ty a
                     => InferKindInput e w s r m ki ty a
recordInferKindRules =
  InferKindInput
    [InferKindRecurse inferTyRecord]
