{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Pair.Rules.Kind.Infer.SyntaxDirected (
    PairInferKindContext
  , pairInferKindRules
  ) where

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)
import Data.Functor.Classes (Eq1)

import Ast.Kind
import Ast.Type
import Ast.Error.Common
import Rules.Kind.Infer.SyntaxDirected

import Fragment.KiBase.Ast.Kind
import Fragment.Pair.Ast.Type

inferTyPair :: (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyPair ki ty)
            => (Type ki ty a -> m (Kind ki))
            -> Type ki ty a
            -> Maybe (m (Kind ki))
inferTyPair inferFn ty = do
  (ty1, ty2) <- preview _TyPair ty
  return $ do
    let ki = review _KiBase()
    mkCheckKind inferFn ty1 ki
    mkCheckKind inferFn ty2 ki
    return . review _KiBase $ ()

type PairInferKindContext e w s r m ki ty a = (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyPair ki ty)

pairInferKindRules :: PairInferKindContext e w s r m ki ty a
                   => InferKindInput e w s r m ki ty a
pairInferKindRules =
  InferKindInput
    [InferKindRecurse inferTyPair]
