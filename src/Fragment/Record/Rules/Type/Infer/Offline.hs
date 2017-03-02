{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Record.Rules.Type.Infer.Offline (
    RecordInferContext
  , recordInferRules
  ) where

import Data.List (sortOn)

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Type.Infer.Offline
import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Record.Ast.Type
import Fragment.Record.Ast.Error
import Fragment.Record.Ast.Pattern
import Fragment.Record.Ast.Term

equivRecord :: AsTyRecord ki ty => (Type ki ty a -> Type ki ty a -> Bool) -> Type ki ty a -> Type ki ty a -> Maybe Bool
equivRecord equivFn ty1 ty2 = do
  rs1 <- preview _TyRecord ty1
  rs2 <- preview _TyRecord ty2
  let f = fmap snd . sortOn fst
  return . and $ zipWith equivFn (f rs1) (f rs2)

inferTmRecord :: (Monad m, AsTyRecord ki ty, AsTmRecord ki ty pt tm) => (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmRecord inferFn tm = do
  tms <- preview _TmRecord tm
  return $ do
    tys <- traverse (traverse inferFn) tms
    return $ review _TyRecord tys

inferTmRecordIx :: (MonadError e m, AsExpectedTyRecord e ki ty a, AsRecordNotFound e, AsTyRecord ki ty, AsTmRecord ki ty pt tm) => (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmRecordIx inferFn tm = do
  (tmT, i) <- preview _TmRecordIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectTyRecord tyT
    lookupRecord tys i

checkRecord :: (MonadError e m, AsExpectedTyRecord e ki ty a, AsRecordNotFound e, AsPtRecord pt, AsTyRecord ki ty) => (Pattern pt a -> Type ki ty a -> m [Type ki ty a]) -> Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkRecord checkFn p ty = do
  ps <- preview _PtRecord p
  return $ do
    -- check for duplicate labels in ps
    ltys <- expectTyRecord ty
    let f (l, lp) = do
          typ <- lookupRecord ltys l
          checkFn lp typ
    fmap mconcat . traverse f $ ps

type RecordInferContext e w s r m ki ty pt tm a = (InferContext e w s r m ki ty pt tm a, AsTyRecord ki ty, AsExpectedTyRecord e ki ty a, AsRecordNotFound e, AsPtRecord pt, AsTmRecord ki ty pt tm)

recordInferRules :: RecordInferContext e w s r m ki ty pt tm a
                => InferInput e w s r m ki ty pt tm a
recordInferRules =
  InferInput
    [ EquivRecurse equivRecord ]
    []
    [ InferRecurse inferTmRecord
    , InferRecurse inferTmRecordIx
    ]
    [ PCheckRecurse checkRecord ]
