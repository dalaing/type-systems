{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Variant.Rules.Infer.SyntaxDirected (
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

equivVariant :: AsTyVariant ki ty => (Type ki ty a -> Type ki ty a -> Bool) -> Type ki ty a -> Type ki ty a -> Maybe Bool
equivVariant equivFn ty1 ty2 = do
  vs1 <- preview _TyVariant ty1
  vs2 <- preview _TyVariant ty2
  let f = fmap snd . sortOn fst . N.toList
  return . and $ zipWith equivFn (f vs1) (f vs2)

inferTmVariant :: (Eq a, EqRec (ty ki), MonadError e m, AsExpectedTyVariant e ki ty a, AsVariantNotFound e, AsExpectedEq e ki ty a, AsTyVariant ki ty, AsTmVariant ki ty pt tm) => (Type ki ty a -> Type ki ty a -> Bool) -> (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmVariant tyEquiv inferFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  return $ do
    tyL <- inferFn tmV
    tys <- expectTyVariant ty
    tyV <- lookupVariant tys l
    expectEq tyEquiv tyL tyV
    return ty

checkVariant :: (MonadError e m, AsExpectedTyVariant e ki ty a, AsVariantNotFound e, AsPtVariant pt, AsTyVariant ki ty) => (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkVariant checkFn p ty = do
  (lV, pV) <- preview _PtVariant p
  return $ do
    vs <- expectTyVariant ty
    tyV <- lookupVariant vs lV
    checkFn pV tyV

type VariantInferContext e w s r m ki ty pt tm a = (InferContext e w s r m ki ty pt tm a, AsTyVariant ki ty, AsExpectedTyVariant e ki ty a, AsVariantNotFound e, AsExpectedEq e ki ty a, AsPtVariant pt, AsTmVariant ki ty pt tm)

variantInferRules :: VariantInferContext e w s r m ki ty pt tm a
                => InferInput e w s r m ki ty pt tm a
variantInferRules =
  InferInput
    [ EquivRecurse equivVariant ]
    [ InferTyEquivRecurse inferTmVariant ]
    [ PCheckRecurse checkVariant ]
