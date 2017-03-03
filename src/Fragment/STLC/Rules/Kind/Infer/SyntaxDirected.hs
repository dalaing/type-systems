{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.STLC.Rules.Kind.Infer.SyntaxDirected (
    STLCKindRulesContext
  , stlcKindRules
  ) where

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)
import Data.Functor.Classes (Eq1)

import Ast.Kind
import Ast.Type
import Ast.Error.Common
import Rules.Kind.Infer.SyntaxDirected

import Fragment.KiBase.Ast.Kind
import Fragment.STLC.Ast.Type

inferTyArr :: (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTySTLC ki ty)
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

type STLCKindRulesContext e s w r m ki ty a = (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTySTLC ki ty)

stlcKindRules :: STLCKindRulesContext e s w r m ki ty a
              => KindRulesInput e s w r m ki ty a
stlcKindRules =
  KindRulesInput
    [InferKindRecurse inferTyArr]
