{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Rules (
    RulesIn(..)
  , RulesOut(..)
  ) where

import Data.Proxy
import GHC.Exts (Constraint)

import qualified Rules.Infer.SyntaxDirected as SD
import Rules.Infer.Unification
import qualified Rules.Infer.Unification.Offline as UO
import Rules.Term

import Ast.Kind
import Ast.Type
import Ast.Error
import Ast.Error.Common
import Ast.Warning
import Ast.Pattern
import Ast.Term

class TLAppend (xs :: [k]) (ys :: [k]) where
  type Append xs ys :: [k]

instance TLAppend '[] ys where
  type Append '[] ys = ys

instance TLAppend xs ys => TLAppend (x ': xs) ys where
  type Append (x ': xs) ys = x ': (Append xs ys)

class RulesIn (k :: j) where
  type RuleInferSyntaxContext e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: Constraint
  type RuleInferOfflineContext e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: Constraint
  type RuleTermContext (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: Constraint
  type KindList k :: [* -> *]
  type TypeList k :: [(* -> *) -> (* -> *) -> * -> *]
  type ErrorList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: [*]
  type WarningList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: [*]
  type PatternList k :: [(* -> *) -> * -> *]
  type TermList k :: [(* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *]

  inferSyntaxInput :: (SD.InferContext e w s r m ki ty pt tm a , RuleInferSyntaxContext e w s r m ki ty pt tm a k) => Proxy k -> SD.InferInput e w s r m ki ty pt tm a
  inferOfflineInput :: (UO.InferContext e w s r m ki ty pt tm a , RuleInferOfflineContext e w s r m ki ty pt tm a k) => Proxy k -> UO.InferInput e w s r m ki ty pt tm a
  termInput :: (TermContext ki ty pt tm a, RuleTermContext ki ty pt tm a k) => Proxy k -> TermInput ki ty pt tm a

instance RulesIn '[] where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a '[] = (() :: Constraint)
  type RuleInferOfflineContext e w s r m ki ty pt tm a '[] = (() :: Constraint)
  type RuleTermContext ki ty pt tm a '[] = (() :: Constraint)
  type KindList '[] = '[]
  type TypeList '[] = '[]
  type ErrorList ki ty pt tm a '[] = '[ErrUnknownTypeError, ErrOccursError ki ty a, ErrUnificationMismatch ki ty a]
  type WarningList ki ty pt tm a '[] = '[]
  type PatternList '[] = '[]
  type TermList '[] = '[]

  inferSyntaxInput _ = mempty
  inferOfflineInput _ = mempty
  termInput _ = mempty

instance (RulesIn k, RulesIn ks) => RulesIn (k ': ks) where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a (k ': ks) = (RuleInferSyntaxContext e w s r m ki ty pt tm a k, RuleInferSyntaxContext e w s r m ki ty pt tm a ks)
  type RuleInferOfflineContext e w s r m ki ty pt tm a (k ': ks) = (RuleInferOfflineContext e w s r m ki ty pt tm a k, RuleInferOfflineContext e w s r m ki ty pt tm a ks)
  type RuleTermContext ki ty pt tm a (k ': ks) = (RuleTermContext ki ty pt tm a k, RuleTermContext ki ty pt tm a ks)
  type KindList (k ': ks) = Append (KindList k) (KindList ks)
  type TypeList (k ': ks) = Append (TypeList k) (TypeList ks)
  type ErrorList ki ty pt tm a (k ': ks) = Append (ErrorList ki ty pt tm a k) (ErrorList ki ty pt tm a ks)
  type WarningList ki ty pt tm a (k ': ks) = Append (WarningList ki ty pt tm a k) (WarningList ki ty pt tm a ks)
  type PatternList (k ': ks) = Append (PatternList k) (PatternList ks)
  type TermList (k ': ks) = Append (TermList k) (TermList ks)

  inferSyntaxInput _ = inferSyntaxInput (Proxy :: Proxy k) `mappend` inferSyntaxInput (Proxy :: Proxy ks)
  inferOfflineInput _ = inferOfflineInput (Proxy :: Proxy k) `mappend` inferOfflineInput (Proxy :: Proxy ks)
  termInput _ = termInput (Proxy :: Proxy k) `mappend` termInput (Proxy :: Proxy ks)

class RulesOut (k :: j) where

  type RKindF k :: * -> *
  type RTypeF k :: ((* -> *) -> (* -> *) -> * -> *)
  type RPatternF k :: ((* -> *) -> * -> *)
  type RTermF k :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)

  type RType k :: (* -> *)
  type RError (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: *
  type RWarning (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: *
  type RPattern k :: (* -> *)
  type RTerm k :: (* -> *)

  inferSyntaxOutput :: (SD.InferContext e w s r m ki ty pt tm a, RuleInferSyntaxContext e w s r m ki ty pt tm a k) => Proxy k -> SD.InferOutput e w s r m ki ty pt tm a
  inferOfflineOutput :: (UO.InferContext e w s r m ki ty pt tm a, RuleInferOfflineContext e w s r m ki ty pt tm a k) => Proxy k -> UO.InferOutput e w s r m ki ty pt tm a
  termOutput :: (TermContext ki ty pt tm a, RuleTermContext ki ty pt tm a k) => Proxy k -> TermOutput ki ty pt tm a

instance RulesIn k => RulesOut (k :: j) where

  type RKindF k = KiSum (KindList k)
  type RTypeF k = TySum (TypeList k)
  type RPatternF k = PtSum (PatternList k)
  type RTermF k = TmSum (TermList k)

  type RType k = Type (RKindF k) (RTypeF k)
  type RError ki ty pt tm a k = ErrSum (ErrorList ki ty pt tm a k)
  type RWarning ki ty pt tm a k = WarnSum (WarningList ki ty pt tm a k)
  type RPattern k = Pattern (RPatternF k)
  type RTerm k = Term (RKindF k) (RTypeF k) (RPatternF k) (RTermF k)

  inferSyntaxOutput = SD.prepareInfer . inferSyntaxInput
  inferOfflineOutput = UO.prepareInfer . inferOfflineInput
  termOutput = prepareTerm . termInput
