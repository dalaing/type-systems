{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.HM.Rules.Kind.Infer.SyntaxDirected (
    HMKindRulesContext
  , hmKindRules
  ) where

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)
import Data.Functor.Classes (Eq1)

import Ast.Kind
import Ast.Type
import Ast.Error.Common
import Rules.Kind.Infer.SyntaxDirected

import Fragment.KiBase.Ast.Kind
import Fragment.HM.Ast.Type

inferTyArr :: (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyHM ki ty)
            => (Type ki ty a -> m (Kind ki))
            -> Type ki ty a
            -> Maybe (m (Kind ki))
inferTyArr inferFn ty = do
  (ty1, ty2) <- preview _TyArr ty
  return $ do
    let ki = review _KiBase()
    mkCheckKind inferFn ty1 ki
    mkCheckKind inferFn ty2 ki
    return . review _KiBase $ ()

type HMKindRulesContext e s w r m ki ty a = (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyHM ki ty)

hmKindRules :: HMKindRulesContext e s w r m ki ty a
              => KindRulesInput e s w r m ki ty a
hmKindRules =
  KindRulesInput
    [InferKindRecurse inferTyArr]
