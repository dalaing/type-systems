{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Bool.Rules.Infer.SyntaxDirected (
    BoolInferContext
  , boolInferRules
  ) where

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Infer
import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Error.Common
import Data.Functor.Rec

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

equivBool :: AsTyBool ki ty => Type ki ty a -> Type ki ty a -> Maybe Bool
equivBool ty1 ty2 = do
  _ <- preview _TyBool ty1
  _ <- preview _TyBool ty2
  return True

inferBool :: (Monad m, AsTyBool ki ty, AsTmBool ki ty pt tm)
         => Term ki ty pt tm a
         -> Maybe (m (Type ki ty a))
inferBool tm = do
  _ <- preview _TmBool tm
  return . return . review _TyBool $ ()

inferAnd :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpected e ki ty a, AsTyBool ki ty, AsTmBool ki ty pt tm)
         => (Type ki ty a -> Type ki ty a -> Bool)
         -> (Term ki ty pt tm a -> m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (m (Type ki ty a))
inferAnd tyEquiv inferFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  return $ do
    let ty = review _TyBool ()
    mkCheck tyEquiv inferFn tm1 ty
    mkCheck tyEquiv inferFn tm2 ty
    return ty

inferOr :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpected e ki ty a, AsTyBool ki ty, AsTmBool ki ty pt tm)
         => (Type ki ty a -> Type ki ty a -> Bool)
         -> (Term ki ty pt tm a -> m (Type ki ty a))
         -> Term ki ty pt tm a
         -> Maybe (m (Type ki ty a))
inferOr tyEquiv inferFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  return $ do
    let ty = review _TyBool ()
    mkCheck tyEquiv inferFn tm1 ty
    mkCheck tyEquiv inferFn tm2 ty
    return ty

checkBool :: (Eq a, EqRec (ty ki), MonadError e m, AsUnexpected e ki ty a, AsPtBool pt, AsTyBool ki ty) => (Type ki ty a -> Type ki ty a -> Bool) -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkBool tyEquiv p ty = do
  _ <- preview _PtBool p
  return $ do
    let tyB = review _TyBool ()
    expect tyEquiv (ExpectedType tyB) (ActualType ty)
    return []

type BoolInferContext e w s r m ki ty pt tm a = (InferContext e w s r m ki ty pt tm a, AsTyBool ki ty, AsPtBool pt, AsTmBool ki ty pt tm)

boolInferRules :: BoolInferContext e w s r m ki ty pt tm a
              => InferInput e w s r m ki ty pt tm a
boolInferRules =
  InferInput
    [ EquivBase equivBool ]
    [ InferBase inferBool
    , InferTyEquivRecurse inferAnd
    , InferTyEquivRecurse inferOr
    ]
    [ PCheckTyEquiv checkBool ]
