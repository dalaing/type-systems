{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.Int.Rules.Type.Infer.SyntaxDirected (
    ISyntaxInt
  ) where

import Control.Lens (review)

import Rules.Type.Infer.SyntaxDirected (ISyntax)

import Fragment.Int.Ast.Type
import Fragment.Int.Rules.Type.Infer.Common

data ISyntaxInt

instance IntInferTypeHelper ISyntax ISyntaxInt where
  type IntInferTypeHelperConstraint e w s r m ki ty a ISyntax ISyntaxInt =
    ( AsTyInt ki ty
    , Monad m
    )

  createInt _ _ _ =
    return . review _TyInt $ ()
