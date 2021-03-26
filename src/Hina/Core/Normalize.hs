module Hina.Core.Normalize where

import           Control.Monad.Freer        (Eff, Member)
import           Control.Monad.Freer.Reader (Reader)
import           Control.Monad.Freer.State  (State)
import           Data.Maybe                 (isJust)
import           Hina.Core                  (Param (Param),
                                             Term (TApp, TBind, TCallVar, TLam, TPi, TProj, TSigma, TTup, TUniv),
                                             TermApp (TermApp),
                                             TermCallVar (TermCallVar),
                                             TermLam (TermLam),
                                             TermProj (TermProj),
                                             TermTup (TermTup), aTerm, dBody)
import           Hina.Core.Substitute       (subst)
import           Hina.Mapping               (CoreMapping, askCoreVar)

isWhnf :: Member (Reader CoreMapping) m => Term -> Eff m Bool
isWhnf term = case term of
  TApp (TermApp fn _) -> case fn of
    TLam _ -> pure True
    _      -> isWhnf fn
  TLam _ -> pure True
  TPi _ -> pure True
  TTup _ -> pure True
  TSigma _ -> pure True
  TProj (TermProj tup _) -> case tup of
    TTup _ -> pure True
    _      -> isWhnf tup
  TUniv _ -> pure True
  TBind _ -> pure True
  TCallVar (TermCallVar ref) -> do
    coreVar <- askCoreVar ref
    pure $ isJust (dBody coreVar)

normalizeToWhnf :: Member (Reader CoreMapping) m => Term -> Eff m Term
normalizeToWhnf term = case term of
  TApp (TermApp fn arg) -> case fn of
    TLam (TermLam bind body) ->
      normalizeToWhnf $ subst bind (aTerm arg) body
    _ -> do
      fnIsWhnf <- isWhnf fn
      if fnIsWhnf then pure term else do
        fn' <- normalizeToWhnf fn
        normalizeToWhnf (TApp (TermApp fn' arg))
  TLam _ -> pure term
  TPi _ -> pure term
  TTup _ -> pure term
  TSigma _ -> pure term
  TProj (TermProj tup toLeft) -> case tup of
    TTup (TermTup left right) ->
      normalizeToWhnf if toLeft then left else right
    _ -> do
      tupIsWhnf <- isWhnf tup
      if tupIsWhnf then pure term else do
        tup' <- normalizeToWhnf tup
        normalizeToWhnf (TProj (TermProj tup' toLeft))
  TUniv _ -> pure term
  TBind _ -> pure term
  TCallVar (TermCallVar ref) -> do
    coreVar <- askCoreVar ref
    case dBody coreVar of
      Nothing -> pure term
      Just x  -> pure x
