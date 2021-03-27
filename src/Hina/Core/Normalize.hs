module Hina.Core.Normalize where

import           Control.Monad.Freer        (Eff, Member)
import           Control.Monad.Freer.Reader (Reader)
import           Hina.Core                  (Term (TApp, TBind, TCallVar, TLam, TPi, TProj, TSigma, TTup, TUniv),
                                             TermApp (TermApp),
                                             TermCallVar (TermCallVar),
                                             TermLam (TermLam),
                                             TermProj (TermProj),
                                             TermTup (TermTup), aTerm, dBody)
import           Hina.Core.Substitute       (subst)
import           Hina.Mapping               (CoreMapping, askCoreVar)

normalizeToWhnf :: Member (Reader CoreMapping) m => Term -> Eff m Term
normalizeToWhnf term = fst <$> normalizeToWhnf' term

normalizeToWhnf' :: Member (Reader CoreMapping) m => Term -> Eff m (Term, Bool)
normalizeToWhnf' term = case term of
  TApp (TermApp fn arg) -> case fn of
    TLam (TermLam bind body) -> do
      whnf <- normalizeToWhnf $ subst bind (aTerm arg) body
      pure (whnf, False)
    _ -> do
      (fn', fnIsWhnf) <- normalizeToWhnf' fn
      if fnIsWhnf then pure (term, True) else do
        whnf <- normalizeToWhnf (TApp (TermApp fn' arg))
        pure (whnf, False)
  TLam _ -> pure (term, True)
  TPi _ -> pure (term, True)
  TTup _ -> pure (term, True)
  TSigma _ -> pure (term, True)
  TProj (TermProj tup toLeft) -> case tup of
    TTup (TermTup left right) -> do
      whnf <- normalizeToWhnf if toLeft then left else right
      pure (whnf, False)
    _ -> do
      (tup', tupIsWhnf) <- normalizeToWhnf' tup
      if tupIsWhnf then pure (term, True) else do
        whnf <- normalizeToWhnf (TProj (TermProj tup' toLeft))
        pure (whnf, False)
  TUniv _ -> pure (term, True)
  TBind _ -> pure (term, True)
  TCallVar (TermCallVar ref) -> do
    coreVar <- askCoreVar ref
    case dBody coreVar of
      Nothing -> pure (term, True)
      Just x  -> pure (x, False)
