{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Record.Rules.Infer (
    RecordInferContext
  , recordInferRules
  ) where

import Data.List (sortOn)

import Control.Monad.Except (MonadError)
import Control.Lens (review, preview)

import Rules.Infer
import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Record.Ast.Type
import Fragment.Record.Ast.Error
import Fragment.Record.Ast.Pattern
import Fragment.Record.Ast.Term

equivRecord :: AsTyRecord ty => (Type ty a -> Type ty a -> Bool) -> Type ty a -> Type ty a -> Maybe Bool
equivRecord equivFn ty1 ty2 = do
  rs1 <- preview _TyRecord ty1
  rs2 <- preview _TyRecord ty2
  let f = fmap snd . sortOn fst
  return . and $ zipWith equivFn (f rs1) (f rs2)

inferTmRecord :: (Monad m, AsTyRecord ty, AsTmRecord ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmRecord inferFn tm = do
  tms <- preview _TmRecord tm
  return $ do
    tys <- traverse (traverse inferFn) tms
    return $ review _TyRecord tys

inferTmRecordIx :: (MonadError e m, AsExpectedTyRecord e ty a, AsRecordNotFound e, AsTyRecord ty, AsTmRecord ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmRecordIx inferFn tm = do
  (tmT, i) <- preview _TmRecordIx tm
  return $ do
    tyT <- inferFn tmT
    tys <- expectTyRecord tyT
    lookupRecord tys i

checkRecord :: (MonadError e m, AsExpectedTyRecord e ty a, AsRecordNotFound e, AsPtRecord pt, AsTyRecord ty) => (Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkRecord checkFn p ty = do
  ps <- preview _PtRecord p
  return $ do
    -- check for duplicate labels in ps
    ltys <- expectTyRecord ty
    let f (l, lp) = do
          typ <- lookupRecord ltys l
          checkFn lp typ
    fmap mconcat . traverse f $ ps

type RecordInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, AsTyRecord ty, AsExpectedTyRecord e ty a, AsRecordNotFound e, AsPtRecord pt, AsTmRecord ty pt tm)

recordInferRules :: RecordInferContext e w s r m ty pt tm a
                => InferInput e w s r m ty pt tm a
recordInferRules =
  InferInput
    [ EquivRecurse equivRecord ]
    [ InferRecurse inferTmRecord
    , InferRecurse inferTmRecordIx
    ]
    [ PCheckRecurse checkRecord ]
