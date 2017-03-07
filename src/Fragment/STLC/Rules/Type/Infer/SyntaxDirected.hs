{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.STLC.Rules.Type.Infer.SyntaxDirected (
    STLCInferTypeContext
  , stlcInferTypeRules
  ) where

import Bound (instantiate1)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped)

import Rules.Type.Infer.SyntaxDirected
import Ast.Type
import Ast.Term
import Ast.Term.Var
import Ast.Error.Common
import Context.Term
import Data.Functor.Rec

import Fragment.STLC.Ast.Type
import Fragment.STLC.Ast.Error
import Fragment.STLC.Ast.Term

inferTmLam :: (Ord a, AstBound ki ty pt tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, AsTySTLC ki ty, AsTmSTLC ki ty pt tm, HasTermContext r ki ty a) => (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmLam inferFn tm = do
  (tyArg, s) <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: (Eq a, EqRec (ty ki), MonadError e m, AsTySTLC ki ty, AsTmSTLC ki ty pt tm, AsExpectedTyArr e ki ty a, AsExpectedTypeEq e ki ty a)
           => (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectTypeEq tyArg tyX
    return tyRet

type STLCInferTypeContext e w s r m ki ty pt tm a = (Ord a, InferTypeContext e w s r m ki ty pt tm a, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ki ty a, AsTySTLC ki ty, AsExpectedTypeEq e ki ty a, AsExpectedTyArr e ki ty a, AsTmSTLC ki ty pt tm)

stlcInferTypeRules :: STLCInferTypeContext e w s r m ki ty pt tm a
               => InferTypeInput e w s r m ki ty pt tm a
stlcInferTypeRules =
  InferTypeInput
    [ InferTypeRecurse inferTmLam
    , InferTypeRecurse inferTmApp
    ]
    []
