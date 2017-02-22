{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Case.Rules.Infer (
    CaseInferContext
  , caseInferRules
  ) where

import Control.Monad (replicateM)
import Data.Foldable (toList)

import Bound (instantiate)
import Bound.Scope (bindings)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)

import Rules.Infer
import Ast.Type
import Ast.Error.Common
import Ast.Pattern
import Ast.Term
import Ast.Term.Var
import Context.Term

import Fragment.Case.Ast.Error
import Fragment.Case.Ast.Term

inferTmCase :: CaseInferContext e s r m ty pt tm a => (Term ty pt tm a -> m (Type ty a)) -> (Pattern pt a -> Type ty a -> m [Type ty a]) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmCase inferFn checkFn tm = do
  (tmC, alts) <- preview _TmCase tm
  return $ do
    let go ty (Alt p s) = do
          p' <- expectPattern p

          let vp = toList p'
          checkForDuplicatedPatternVariables vp
          -- TODO possibly turn this into a warning
          checkForUnusedPatternVariables vp (bindings s)

          vs <- replicateM (length p') freshTmVar
          tys <- checkFn p' ty
          let setup = foldr (.) id (zipWith insertTerm vs tys)

          let tm' = review _Wrapped .
                    instantiate (review (_Unwrapped . _TmVar) . (vs !!)) $
                    s
          local (termContext %~ setup) $ inferFn tm'
    tyC <- inferFn tmC
    tys <- mapM (go tyC) alts
    expectAllEq tys

type CaseInferContext e s r m ty pt tm a = (Ord a, AstBound ty pt tm, InferContext e s r m ty pt tm a, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a, AsExpectedPattern e ty pt tm a, AsDuplicatedPatternVariables e a, AsUnusedPatternVariables e a, AsExpectedAllEq e ty a, AsTmCase ty pt tm)

caseInferRules :: CaseInferContext e s r m ty pt tm a
                => InferInput e s r m ty pt tm a
caseInferRules =
  InferInput
    [ InferPCheck inferTmCase ]
    []
