{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.STLC.Rules.Infer (
    STLCInferContext
  , stlcInferRules
  ) where

import Bound (instantiate1)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped)

import Rules.Infer
import Ast.Type
import Ast.Term
import Ast.Term.Var
import Ast.Error.Common
import Context.Term
import Data.Functor.Rec

import Fragment.STLC.Ast.Type
import Fragment.STLC.Ast.Error
import Fragment.STLC.Ast.Term

equivArr :: AsTySTLC ty => (Type ty a -> Type ty a -> Bool) -> Type ty a -> Type ty a -> Maybe Bool
equivArr equivFn ty1 ty2 = do
  (p1a, p1b) <- preview _TyArr ty1
  (p2a, p2b) <- preview _TyArr ty2
  return $ equivFn p1a p2a && equivFn p1b p2b

inferTmLam :: (Ord a, AstBound ty pt tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, AsTySTLC ty, AsTmSTLC ty pt tm, HasTermContext r ty a) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmLam inferFn tm = do
  (tyArg, s) <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: (Eq a, EqRec ty, MonadError e m, AsTySTLC ty, AsTmSTLC ty pt tm, AsExpectedTyArr e ty a, AsExpectedEq e ty a) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectEq tyArg tyX
    return tyRet

type STLCInferContext e w s r m ty pt tm a = (Ord a, InferContext e w s r m ty pt tm a, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a, AsTySTLC ty, AsExpectedEq e ty a, AsExpectedTyArr e ty a, AsTmSTLC ty pt tm)

stlcInferRules :: STLCInferContext e w s r m ty pt tm a
               => InferInput e w s r m ty pt tm a
stlcInferRules =
  InferInput
    [ EquivRecurse equivArr ]
    [ InferRecurse inferTmLam
    , InferRecurse inferTmApp
    ]
    []
