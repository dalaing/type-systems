{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Fragment.Case.Rules.Type.Infer.Common (
    CaseInferTypeContext
  , CaseHelper(..)
  , inferTypeInput
  ) where

import Control.Monad (replicateM)
import Data.Foldable (toList)

import Bound (instantiate)
import Bound.Scope (bindings)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Data.List.NonEmpty (NonEmpty)

import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Term.Var
import Context.Term
import Data.Functor.Rec

import Fragment.Case.Ast.Error
import Fragment.Case.Ast.Warning
import Fragment.Case.Ast.Term

import Rules.Type.Infer.Common

type CaseInferTypeContext e w s r m mi ki ty pt tm a = (Ord a, EqRec (ty ki), AstBound ki ty pt tm, MonadState s mi, HasTmVarSupply s, ToTmVar a, MonadReader r mi, HasTermContext r ki ty a, MonadWriter [w] m, AsUnusedPatternVariables w a, AsShadowingPatternVariables w a, AsTmCase ki ty pt tm, MonadError e mi, AsDuplicatedPatternVariables e a, AsExpectedPattern e ki ty pt tm a)

data CaseHelper m mi ki ty a =
  CaseHelper {
    chLift :: forall x. m x -> mi x
  , chExpectTypeAllEq :: NonEmpty (Type ki ty a) -> mi (Type ki ty a)
  }

inferTmCase :: CaseInferTypeContext e w s r m mi ki ty pt tm a
            => CaseHelper m mi ki ty a
            -> (Term ki ty pt tm a -> mi (Type ki ty a))
            -> (Pattern pt a -> Type ki ty a -> mi [Type ki ty a])
            -> Term ki ty pt tm a
            -> Maybe (mi (Type ki ty a))
inferTmCase (CaseHelper lift expectTypeAllEq) inferFn checkFn tm = do
  (tmC, alts) <- preview _TmCase tm
  return $ do
    let
      go ty (Alt p s) = do
          p' <- expectPattern p

          let vp = toList p'
          checkForDuplicatedPatternVariables vp
          let scopeBindings = bindings s
          lift $ checkForUnusedPatternVariables vp scopeBindings
          contextBindings <- lookupTermBindings
          -- this won't fire at the moment, because bound is taking care of shadowing for us
          -- we'll need to track a bit more info about the original form of the AST before this works
          lift $ checkForShadowingPatternVariables vp contextBindings

          vs <- replicateM (length p') freshTmVar
          tys <- checkFn p' ty
          let setup = foldr (.) id (zipWith insertTerm vs tys)

          let tm' = review _Wrapped .
                    instantiate (review (_Unwrapped . _TmVar) . (vs !!)) $
                    s
          local (termContext %~ setup) $ inferFn tm'
    tyC <- inferFn tmC
    tys <- mapM (go tyC) alts
    expectTypeAllEq tys

inferTypeInput :: CaseInferTypeContext e w s r m mi ki ty pt tm a
               => CaseHelper m mi ki ty a
               -> InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput ch =
  InferTypeInput [] [ InferTypePCheck $ inferTmCase ch ] []
