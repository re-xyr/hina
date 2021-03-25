module Hina.Tyck.Unify where

import           Control.Monad             (when)
import           Control.Monad.Freer       (Eff, Member, Members)
import           Control.Monad.Freer.Error (Error, runError, throwError)
import           Control.Monad.Freer.State (State, evalState, get, modify)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import           Hina.Core                 (Arg (Arg), Param (Param),
                                            Term (TApp, TBind, TPi, TProj, TSigma, TUniv),
                                            TermApp (TermApp),
                                            TermBind (TermBind),
                                            TermPi (TermPi),
                                            TermProj (TermProj),
                                            TermSigma (TermSigma),
                                            TermUniv (TermUniv))
import           Hina.Core.Normalize       (normalizeToWhnf)
import           Hina.Core.Substitute      (subst)
import           Hina.Ref                  (RefBind (rName), freshBind)
import           Hina.Tyck.Context         (TyckEff, getLocal, withLocal)

type UnifyEff m = (TyckEff m, Members '[State LocalCorrespond, Error ()] m)

type LocalCorrespond = Map.Map RefBind RefBind

withCorrespond :: Member (State LocalCorrespond) m => RefBind -> RefBind -> Eff m a -> Eff m a
withCorrespond x y m = do
  modify (Map.insert y x . Map.insert x y)
  res <- m
  modify @LocalCorrespond (Map.delete x . Map.delete y)
  pure res

getCorrespond :: Member (State LocalCorrespond) m => RefBind -> Eff m RefBind
getCorrespond r = fromMaybe r . Map.lookup r <$> get

unify :: TyckEff m => Term -> Term -> Term -> Eff m Bool
unify ty x y = evalState @LocalCorrespond Map.empty do
  res <- runError @() $ unify' ty x y
  pure case res of
    Left _  -> False
    Right _ -> True

unify' :: UnifyEff m => Term -> Term -> Term -> Eff m ()
unify' ty x y = do
  ty' <- normalizeToWhnf ty
  x' <- normalizeToWhnf x
  y' <- normalizeToWhnf y
  unifyWhnf ty' x' y'

unifyWhnf :: UnifyEff m => Term -> Term -> Term -> Eff m ()
unifyWhnf ty x y = case (ty, x, y) of
  (TPi (TermPi (Param tRef tTyp) tBody), _, _) -> do
    bnd <- freshBind (rName tRef)
    withLocal bnd tTyp do
      let dummy = TBind $ TermBind bnd
      unify' (subst tRef dummy tBody) (TApp $ TermApp x $ Arg dummy) (TApp $ TermApp x $ Arg dummy)
  (TSigma (TermSigma (Param tRef tTyp) tBody), _, _) -> do
    unify' tTyp (TProj $ TermProj x True) (TProj $ TermProj y True)
    unify' (subst tRef (TProj $ TermProj x True) tBody) (TProj $ TermProj x False) (TProj $ TermProj y False)
  (TUniv _, TPi (TermPi (Param xRef xTyp) xBody), TPi (TermPi (Param yRef yTyp) yBody)) -> do
    unify' (TUniv TermUniv) xTyp yTyp
    withLocal xRef xTyp $ withLocal yRef yTyp $ withCorrespond xRef yRef $
      unify' (TUniv TermUniv) xBody yBody
  (TUniv _, TSigma (TermSigma (Param xRef xTyp) xBody), TSigma (TermSigma (Param yRef yTyp) yBody)) -> do
    unify' (TUniv TermUniv) xTyp yTyp
    withLocal xRef xTyp $ withLocal yRef yTyp $ withCorrespond xRef yRef $
      unify' (TUniv TermUniv) xBody yBody
  (TUniv _, TUniv _, TUniv _) -> pure ()
  (_, _, _) -> do
    ty' <- unifyNeutral x y
    unify' (TUniv TermUniv) ty' ty

unifyNeutral :: UnifyEff m => Term -> Term -> Eff m Term
unifyNeutral x y = case (x, y) of
  (TApp (TermApp xFn (Arg xArg)), TApp (TermApp yFn (Arg yArg))) -> do
    fnTy <- unifyNeutral xFn yFn >>= normalizeToWhnf
    case fnTy of
      TPi (TermPi (Param tRef tTyp) tBody) -> do
        unify' tTyp xArg yArg
        pure $ subst tRef xArg tBody
      _ -> error "Impossible"
  (TProj (TermProj xTup xIsLeft), TProj (TermProj yTup yIsLeft)) -> do
    when (xIsLeft /= yIsLeft) $ throwError ()
    tupTy <- unifyNeutral xTup yTup >>= normalizeToWhnf
    pure case tupTy of
      TPi (TermPi (Param tRef tTyp) tBody) -> if xIsLeft
        then tTyp
        else subst tRef (TProj $ TermProj xTup True) tBody
      _ -> error "Impossible"
  (TBind (TermBind xRef), TBind (TermBind yRef)) -> do
    yRef' <- getCorrespond yRef
    when (xRef /= yRef') $ throwError ()
    getLocal xRef
  _ -> throwError ()
