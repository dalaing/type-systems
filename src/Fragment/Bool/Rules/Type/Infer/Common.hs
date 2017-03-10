{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Bool.Rules.Type.Infer.Common (
    BoolHelper(..)
  , inferTypeInput
  ) where

import Control.Lens (review, preview)

import Ast.Type
import Ast.Pattern
import Ast.Error.Common
import Ast.Term

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

import Rules.Type.Infer.Common

data BoolHelper m ki ty a =
  BoolHelper {
    bhCreateBool :: m (Type ki ty a)
  , bhExpectType :: ExpectedType ki ty a -> ActualType ki ty a -> m ()
  }

inferTypeInput :: (Monad mi, AsTyBool ki ty, AsPtBool pt, AsTmBool ki ty pt tm)
               => BoolHelper mi ki ty a
               -> InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput ih =
  InferTypeInput
    []
    [ InferTypeBase inferTmBool
    , InferTypeRecurse $ inferTmAnd ih
    , InferTypeRecurse $ inferTmOr ih
    ]
    [ PCheckBase $ checkBool ih]

inferTmBool :: (AsTyBool ki ty, AsTmBool ki ty pt tm, Monad m)
           => Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmBool tm = do
  _ <- preview _TmBool tm
  return . return . review _TyBool $ ()

inferTmAnd :: (AsTyBool ki ty, AsTmBool ki ty pt tm, Monad m)
           => BoolHelper m ki ty a
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmAnd (BoolHelper createBool expectType) inferFn tm = do
  (tm1, tm2) <- preview _TmAnd tm
  return $ do
    let ty = review _TyBool ()
    ty1 <- inferFn tm1
    expectType (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType (ExpectedType ty) (ActualType ty2)
    tyV <- createBool
    expectType (ExpectedType ty) (ActualType tyV)
    return tyV

inferTmOr :: (AsTyBool ki ty, AsTmBool ki ty pt tm, Monad m)
          => BoolHelper m ki ty a
          -> (Term ki ty pt tm a -> m (Type ki ty a))
          -> Term ki ty pt tm a
          -> Maybe (m (Type ki ty a))
inferTmOr (BoolHelper createBool expectType) inferFn tm = do
  (tm1, tm2) <- preview _TmOr tm
  return $ do
    let ty = review _TyBool ()
    ty1 <- inferFn tm1
    expectType (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType (ExpectedType ty) (ActualType ty2)
    tyV <- createBool
    expectType (ExpectedType ty) (ActualType tyV)
    return tyV

checkBool :: (AsPtBool pt, AsTyBool ki ty, Monad m)
         => BoolHelper m ki ty a
         -> Pattern pt a
         -> Type ki ty a
         -> Maybe (m [Type ki ty a])
checkBool (BoolHelper _ expectType) p ty = do
  _ <- preview _PtBool p
  return $ do
    let tyI = review _TyBool ()
    expectType (ExpectedType tyI) (ActualType ty)
    return []
