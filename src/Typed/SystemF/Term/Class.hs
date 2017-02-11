{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
module Typed.SystemF.Term.Class (
    AsTerm(..)
  ) where

import Control.Lens

class AsTerm (ty :: * -> *) tm | tm -> ty where
  _TmVar   :: Prism' (tm a) a
  _TmLam   :: Eq a => Prism' (a, tm a) (a, ty a, tm a)
  _TmApp   :: Prism' (tm a) (tm a, tm a)
  _TmLamTy :: Eq a => Prism' (a, tm a) (a, tm a)
  _TmAppTy :: Prism' (tm a) (tm a, ty a)
  _TmInt   :: Prism' (tm a) Int
  _TmAdd   :: Prism' (tm a) (tm a, tm a)

  reduceLamApp :: tm a -> Maybe (tm a)
  reduceLamTyAppTy :: tm a -> Maybe (tm a)
