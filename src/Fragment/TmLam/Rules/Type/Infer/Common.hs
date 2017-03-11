{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TmLam.Rules.Type.Infer.Common (
    TmLamInferTypeContext
  , TmLamHelper(..)
  , inferTypeInput
  ) where

import Bound (Scope, instantiate1)
import Control.Lens (review, (%~))
import Control.Lens.Wrapped (_Wrapped)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)

import Ast.Type
import Ast.Term
import Ast.Term.Var
import Context.Term

import Fragment.TyArr.Ast.Type
import Fragment.TmLam.Ast.Term

import Rules.Type.Infer.Common

data TmLamHelper m ki ty pt tm a =
  TmLamHelper {
    tlExpectTmLam :: Term ki ty pt tm a -> Maybe (m (Type ki ty a, Scope () (Ast ki ty pt tm) (AstVar a)))
  }

inferTmLam :: TmLamInferTypeContext e w s r m mi ki ty pt tm a
           => TmLamHelper mi ki ty pt tm a
           -> (Term ki ty pt tm a -> mi (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (mi (Type ki ty a))
inferTmLam (TmLamHelper expectTmLam) inferFn tm = do
  act <- expectTmLam tm
  return $ do
    (tyArg, s) <- act
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

type TmLamInferTypeContext e w s r (m :: * -> *) mi ki ty pt tm a =
  ( Ord a
  , AsTyArr ki ty
  , AsTmLam ki ty pt tm
  , MonadReader r mi
  , HasTermContext r ki ty a
  , MonadState s mi
  , HasTmVarSupply s
  , ToTmVar a
  )

inferTypeInput :: TmLamInferTypeContext e w s r m mi ki ty pt tm a
               => TmLamHelper mi ki ty pt tm a
               -> InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput lh =
  InferTypeInput
    []
    [InferTypeRecurse $ inferTmLam lh]
    []
