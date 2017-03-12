{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Int.Rules.Type.Infer.Common (
    IntHelper(..)
  , inferTypeInput
  ) where

import Control.Lens (review, preview)

import Ast.Type
import Ast.Pattern
import Ast.Error.Common
import Ast.Term

import Fragment.Int.Ast.Type
import Fragment.Int.Ast.Pattern
import Fragment.Int.Ast.Term

import Rules.Type.Infer.Common

data IntHelper m ki ty a =
  IntHelper {
    ihCreateInt :: m (Type ki ty a)
  , ihExpectType :: ExpectedType ki ty a -> ActualType ki ty a -> m ()
  }

inferTypeInput :: (Monad mi, AsTyInt ki ty, AsPtInt pt, AsTmInt ki ty pt tm)
               => IntHelper mi ki ty a
               -> InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput ih =
  InferTypeInput
    []
    [ InferTypeBase inferTmInt
    , InferTypeRecurse $ inferTmAdd ih
    , InferTypeRecurse $ inferTmSub ih
    , InferTypeRecurse $ inferTmMul ih
    ]
    [ PCheckBase $ checkInt ih]

inferTmInt :: (AsTyInt ki ty, AsTmInt ki ty pt tm, Monad m)
           => Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmInt tm = do
  _ <- preview _TmInt tm
  return . return . review _TyInt $ ()

inferTmAdd :: (AsTyInt ki ty, AsTmInt ki ty pt tm, Monad m)
           => IntHelper m ki ty a
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmAdd (IntHelper createInt expectType) inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    let ty = review _TyInt ()
    ty1 <- inferFn tm1
    expectType (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType (ExpectedType ty) (ActualType ty2)
    tyV <- createInt
    expectType (ExpectedType ty) (ActualType tyV)
    return tyV

inferTmSub :: (AsTyInt ki ty, AsTmInt ki ty pt tm, Monad m)
           => IntHelper m ki ty a
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmSub (IntHelper createInt expectType) inferFn tm = do
  (tm1, tm2) <- preview _TmSub tm
  return $ do
    let ty = review _TyInt ()
    ty1 <- inferFn tm1
    expectType (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType (ExpectedType ty) (ActualType ty2)
    tyV <- createInt
    expectType (ExpectedType ty) (ActualType tyV)
    return tyV

inferTmMul :: (AsTyInt ki ty, AsTmInt ki ty pt tm, Monad m)
           => IntHelper m ki ty a
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmMul (IntHelper createInt expectType) inferFn tm = do
  (tm1, tm2) <- preview _TmMul tm
  return $ do
    let ty = review _TyInt ()
    ty1 <- inferFn tm1
    expectType (ExpectedType ty) (ActualType ty1)
    ty2 <- inferFn tm2
    expectType (ExpectedType ty) (ActualType ty2)
    tyV <- createInt
    expectType (ExpectedType ty) (ActualType tyV)
    return tyV

checkInt :: (AsPtInt pt, AsTyInt ki ty, Monad m)
         => IntHelper m ki ty a
         -> Pattern pt a
         -> Type ki ty a
         -> Maybe (m [Type ki ty a])
checkInt (IntHelper _ expectType) p ty = do
  _ <- preview _PtInt p
  return $ do
    let tyI = review _TyInt ()
    expectType (ExpectedType tyI) (ActualType ty)
    return []
