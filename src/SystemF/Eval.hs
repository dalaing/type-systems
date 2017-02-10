{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module SystemF.Eval (
    value
  , step
  , eval
  ) where

import Data.Foldable (asum)

import Control.Lens

import SystemF.Scratch2

value :: (Eq a, AsTerm ty tm) => a -> tm a -> Maybe (tm a)
value x tm = asum . fmap ($ tm) $ [
    evalTmInt
  , evalTmLam x
  ]

step :: AsTerm ty tm => tm a -> Maybe (tm a)
step tm = asum . fmap ($ tm) $ [
    evalTmAdd1 step
  , evalTmAdd2 step
  , evalTmAddInt
  , evalTmApp1 step
  , evalTmLamApp
  , evalTmAppTy1 step
  , evalTmLamTyAppTy
  ]

eval :: AsTerm ty tm => tm a -> tm a
eval tm =
  case step tm of
    Nothing -> tm
    Just tm' -> eval tm'

-- values

evalTmLam :: Eq a => AsTerm ty tm => a -> tm a -> Maybe (tm a)
evalTmLam x tm = do
  _ <- preview _TmLam (x, tm)
  return $ tm

evalTmInt :: AsTerm ty tm => tm a -> Maybe (tm a)
evalTmInt tm = do
  _ <- preview _TmInt tm
  return tm

-- lam / app

evalTmApp1 :: AsTerm ty tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
evalTmApp1 evalFn tm = do
  (f, x) <- preview _TmApp tm
  f' <- evalFn f
  return $ review _TmApp (f', x)

evalTmLamApp :: AsTerm ty tm => tm a -> Maybe (tm a)
evalTmLamApp = reduceLamApp

-- lamTy / appTy

evalTmAppTy1 :: AsTerm ty tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
evalTmAppTy1 evalFn tm = do
  (f, x) <- preview _TmAppTy tm
  f' <- evalFn f
  return $ review _TmAppTy (f', x)

evalTmLamTyAppTy :: AsTerm ty tm => tm a -> Maybe (tm a)
evalTmLamTyAppTy = reduceLamTyAppTy

-- add

evalTmAdd1 :: AsTerm ty tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
evalTmAdd1 evalFn tm = do
  (x, y) <- preview _TmAdd tm
  x' <- evalFn x
  return $ review _TmAdd (x', y)

evalTmAdd2 :: AsTerm ty tm => (tm a -> Maybe (tm a)) -> tm a -> Maybe (tm a)
evalTmAdd2 evalFn tm = do
  (x, y) <- preview _TmAdd tm
  _ <- preview _TmInt x
  y' <- evalFn y
  return $ review _TmAdd (x, y')

evalTmAddInt :: AsTerm ty tm => tm a -> Maybe (tm a)
evalTmAddInt tm = do
  (x, y) <- preview _TmAdd tm
  x' <- preview _TmInt x
  y' <- preview _TmInt y
  return $ review _TmInt (x' + y')
