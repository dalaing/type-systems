{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Ast (
    AstIn(..)
  , AstOut(..)
  ) where

import Util.TypeList

import Ast.Kind
import Ast.Type
import Ast.Pattern
import Ast.Term

class AstIn (k :: j) where
  type KindList k :: [(* -> *) -> * -> *]
  type TypeList k :: [((* -> *) -> * -> *) -> (* -> *) -> * -> *]
  type TypeSchemeList k :: [((* -> *) -> * -> *) -> (* -> *) -> * -> *]
  type PatternList k :: [(* -> *) -> * -> *]
  type TermList k :: [((* -> *) -> * -> *) -> (((* -> *) -> * -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *]

instance AstIn '[] where
  type KindList '[] = '[]
  type TypeList '[] = '[]
  type TypeSchemeList '[] = '[]
  type PatternList '[] = '[]
  type TermList '[] = '[]

instance (AstIn k, AstIn ks) => AstIn (k ': ks) where
  type KindList (k ': ks) = Append (KindList k) (KindList ks)
  type TypeList (k ': ks) = Append (TypeList k) (TypeList ks)
  type TypeSchemeList (k ': ks) = Append (TypeSchemeList k) (TypeSchemeList ks)
  type PatternList (k ': ks) = Append (PatternList k) (PatternList ks)
  type TermList (k ': ks) = Append (TermList k) (TermList ks)

class AstOut (k :: j) where

  type RKindF k :: ((* -> *) -> * -> *)
  type RTypeF k :: (((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  type RTypeSchemeF k :: (((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  type RPatternF k :: ((* -> *) -> * -> *)
  type RTermF k :: (((* -> *) -> * -> *) -> (((* -> *) -> * -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)

  type RKind k :: (* -> *)
  type RType k :: (* -> *)
  type RTypeScheme k :: (* -> *)
  type RPattern k :: (* -> *)
  type RTerm k :: (* -> *)

instance AstIn k => AstOut (k :: j) where
  type RKindF k = KiSum (KindList k)
  type RTypeF k = TySum (TypeList k)
  type RTypeSchemeF k = TySum (Append (TypeList k) (TypeSchemeList k))
  type RPatternF k = PtSum (PatternList k)
  type RTermF k = TmSum (TermList k)

  type RKind k = Kind (RKindF k)
  type RType k = Type (RKindF k) (RTypeF k)
  type RTypeScheme k = Type (RKindF k) (RTypeSchemeF k)
  type RPattern k = Pattern (RPatternF k)
  type RTerm k = Term (RKindF k) (RTypeF k) (RPatternF k) (RTermF k)
