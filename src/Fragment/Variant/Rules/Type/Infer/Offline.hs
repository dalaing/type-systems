{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Variant.Rules.Type.Infer.Offline (
    VariantInferContext
  , variantInferRules
  ) where

import Control.Monad.Except (MonadError)
import Control.Lens (preview)

import Rules.Type.Infer.Offline
import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common
import Data.Functor.Rec

import Fragment.Variant.Ast.Type
import Fragment.Variant.Ast.Error
import Fragment.Variant.Ast.Pattern
import Fragment.Variant.Ast.Term

inferTmVariant :: (Eq a, EqRec (ty ki), MonadError e m, AsExpectedTyVariant e ki ty a, AsVariantNotFound e, AsExpectedTypeEq e ki ty a, AsTyVariant ki ty, AsTmVariant ki ty pt tm)
               => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
               -> Term ki ty pt tm a
               -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferTmVariant inferFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  return $ do
    tyL <- inferFn tmV
    tys <- expectTyVariant ty
    tyV <- lookupVariant tys l
    expectTypeEq tyL tyV
    return ty

checkVariant :: (MonadError e m, AsExpectedTyVariant e ki ty a, AsVariantNotFound e, AsPtVariant pt, AsTyVariant ki ty) => (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkVariant checkFn p ty = do
  (lV, pV) <- preview _PtVariant p
  return $ do
    vs <- expectTyVariant ty
    tyV <- lookupVariant vs lV
    checkFn pV tyV

type VariantInferContext e w s r m ki ty pt tm a = (InferContext e w s r m ki ty pt tm a, AsTyVariant ki ty, AsExpectedTyVariant e ki ty a, AsVariantNotFound e, AsExpectedTypeEq e ki ty a, AsPtVariant pt, AsTmVariant ki ty pt tm)

variantInferRules :: VariantInferContext e w s r m ki ty pt tm a
                => InferInput e w s r m ki ty pt tm a
variantInferRules =
  InferInput
    []
    [ InferRecurse inferTmVariant ]
    [ PCheckRecurse checkVariant ]
