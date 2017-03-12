{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Rules.Type.Infer.Common (
    InferTypeRule(..)
  , mkInferType
  , mkCheckType'
  , PCheckRule(..)
  , mkPCheck
  , InferTypeInput(..)
  , InferTypeOutput(..)
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Ast.Kind
import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common
import Data.Functor.Rec

import Rules.Unification

data InferTypeRule e w s r m ki ty pt tm a =
    InferTypeBase (Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferTypePCheck ((Term ki ty pt tm a -> m (Type ki ty a)) -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferTypeRecurse ((Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))
  | InferTypeRecurseKind ((Type ki ty a -> m (Kind ki)) -> (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a)))

fixInferTypeRule :: (Type ki ty a -> m (Kind ki))
                 -> (Term ki ty pt tm a -> m (Type ki ty a))
                 -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a])
                 -> InferTypeRule e w s r m ki ty pt tm a
                 -> Term ki ty pt tm a
                 -> Maybe (m (Type ki ty a))
fixInferTypeRule _ _ _ (InferTypeBase f) = f
fixInferTypeRule _ inferFn checkFn (InferTypePCheck f) = f inferFn checkFn
fixInferTypeRule _ inferFn _ (InferTypeRecurse f) = f inferFn
fixInferTypeRule inferKindFn inferTypeFn _ (InferTypeRecurseKind f) = f inferKindFn inferTypeFn

mkInferType :: (MonadError e m, AsUnknownTypeError e)
            => (Type ki ty a -> m (Kind ki))
            -> (Type ki ty a -> Type ki ty a)
            -> (Pattern pt a -> Type ki ty a -> m [Type ki ty a])
            -> [InferTypeRule e w s r m ki ty pt tm a]
            -> Term ki ty pt tm a
            -> m (Type ki ty a)
mkInferType inferKindFn normalizeFn pc rules =
  let
    go tm =
      fmap normalizeFn .
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixInferTypeRule inferKindFn go pc r tm) $
      rules
  in
    go

mkCheckType' :: (Eq a, EqRec (ty ki), Monad m)
            => (ExpectedType ki ty a -> ActualType ki ty a -> m ())
            -> (Term ki ty pt tm a -> m (Type ki ty a))
            -> Term ki ty pt tm a
            -> Type ki ty a
            -> m ()
mkCheckType' expectType inferTypeFn =
  let
    go tm ty = do
      tyAc <- inferTypeFn tm
      expectType (ExpectedType ty) (ActualType tyAc)
  in
    go

data PCheckRule e m pt ki ty a =
    PCheckBase (Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a]))
  | PCheckRecurse ((Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a]))

fixPCheckRule :: (Pattern pt a -> Type ki ty a -> m [Type ki ty a])
              -> PCheckRule e m pt ki ty a
              -> Pattern pt a
              -> Type ki ty a
              -> Maybe (m [Type ki ty a])
fixPCheckRule _ (PCheckBase f) = f
fixPCheckRule pPCheckFn (PCheckRecurse f) = f pPCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e)
         => [PCheckRule e m pt ki ty a]
         -> Pattern pt a
         -> Type ki ty a
         -> m [Type ki ty a]
mkPCheck rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule go r p ty) $
      rules
  in
    go x y

data InferTypeInput e w s r m mi ki ty pt tm a =
  InferTypeInput {
    iiUnifyRules :: [UnificationRule m (Type ki ty) a]
  , iiInferTypeRules :: [InferTypeRule e w s r mi ki ty pt tm a]
  , iiPCheckRules :: [PCheckRule e mi pt ki ty a]
  }

instance Monoid (InferTypeInput e w s r m mi ki ty pt tm a) where
  mempty =
    InferTypeInput mempty mempty mempty
  mappend (InferTypeInput u1 i1 c1) (InferTypeInput u2 i2 c2) =
    InferTypeInput
      (mappend u1 u2)
      (mappend i1 i2)
      (mappend c1 c2)

data InferTypeOutput e w s r m ki ty pt tm a =
  InferTypeOutput {
    ioInfer :: Term ki ty pt tm a -> m (Type ki ty a)
  , ioCheck :: Term ki ty pt tm a -> Type ki ty a -> m ()
  }
