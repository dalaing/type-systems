{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Variant.Rules.Infer (
    VariantInferContext
  , variantInferRules
  ) where

import Data.List (sortOn)

import Control.Monad.Except (MonadError)
import Control.Lens (preview)

import qualified Data.List.NonEmpty as N

import Rules.Infer
import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common
import Data.Functor.Rec

import Fragment.Variant.Ast.Type
import Fragment.Variant.Ast.Error
import Fragment.Variant.Ast.Pattern
import Fragment.Variant.Ast.Term

equivVariant :: AsTyVariant ty => (Type ty a -> Type ty a -> Bool) -> Type ty a -> Type ty a -> Maybe Bool
equivVariant equivFn ty1 ty2 = do
  vs1 <- preview _TyVariant ty1
  vs2 <- preview _TyVariant ty2
  let f = fmap snd . sortOn fst . N.toList
  return . and $ zipWith equivFn (f vs1) (f vs2)

inferTmVariant :: (Eq a, EqRec ty, MonadError e m, AsExpectedTyVariant e ty a, AsVariantNotFound e, AsExpectedEq e ty a, AsTyVariant ty, AsTmVariant ty pt tm) => (Type ty a -> Type ty a -> Bool) -> (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmVariant tyEquiv inferFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  return $ do
    tyL <- inferFn tmV
    tys <- expectTyVariant ty
    tyV <- lookupVariant tys l
    expectEq tyEquiv tyL tyV
    return ty

checkVariant :: (MonadError e m, AsExpectedTyVariant e ty a, AsVariantNotFound e, AsPtVariant pt, AsTyVariant ty) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkVariant checkFn p ty = do
  (lV, pV) <- preview _PtVariant p
  return $ do
    vs <- expectTyVariant ty
    tyV <- lookupVariant vs lV
    checkFn pV tyV

type VariantInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, AsTyVariant ty, AsExpectedTyVariant e ty a, AsVariantNotFound e, AsExpectedEq e ty a, AsPtVariant pt, AsTmVariant ty pt tm)

variantInferRules :: VariantInferContext e w s r m ty pt tm a
                => InferInput e w s r m ty pt tm a
variantInferRules =
  InferInput
    [ EquivRecurse equivVariant ]
    [ InferTyEquivRecurse inferTmVariant ]
    [ PCheckRecurse checkVariant ]
