module Hina.Tyck where

import           Control.Monad             (unless)
import           Control.Monad.Freer       (Eff)
import           Control.Monad.Freer.Error (throwError)
import           Hina.Concrete             (Expr (EApp, ELam, EPi, EProj, ESigma, ETup, EUniv, EVar),
                                            ExprApp (ExprApp),
                                            ExprLam (ExprLam), ExprPi (ExprPi),
                                            ExprProj (ExprProj),
                                            ExprSigma (ExprSigma),
                                            ExprTup (ExprTup),
                                            ExprUniv (ExprUniv),
                                            ExprVar (ExprVar), Stmt (SVar),
                                            StmtVar (StmtVar))
import qualified Hina.Concrete             as E
import           Hina.Core                 (Def (DVar), DefVar (DefVar),
                                            Term (TApp, TBind, TCallVar, TLam, TPi, TProj, TSigma, TTup, TUniv),
                                            TermApp (TermApp),
                                            TermBind (TermBind),
                                            TermCallVar (TermCallVar),
                                            TermLam (TermLam), TermPi (TermPi),
                                            TermProj (TermProj),
                                            TermSigma (TermSigma),
                                            TermTup (TermTup),
                                            TermUniv (TermUniv), dTyp)
import qualified Hina.Core                 as T
import           Hina.Core.Normalize       (normalizeToWhnf)
import           Hina.Core.Substitute      (subst)
import           Hina.Ref                  (Ref (RBind, RGlobal),
                                            RefGlobal (RGVar), freshBind)
import           Hina.Tyck.Context         (TyckEff,
                                            TyckError (TEAppFnNotPi, TEBodyNotUniv, TEInferDiff, TENonInferrable, TEParNotUniv, TEProjTupNotSigma),
                                            getCoreVar, getLocal, setCoreVar,
                                            withLocal)
import           Hina.Tyck.Unify           (unify)

checkExpr :: TyckEff m => Expr Ref -> Term -> Eff m Term
checkExpr expr ty = do
  ty' <- normalizeToWhnf ty
  case (expr, ty') of
    (ELam (ExprLam ref body), TPi (TermPi (T.Param tRef tTyp) tBody)) -> withLocal ref tTyp do
      body' <- checkExpr body (subst tRef (TBind $ TermBind ref) tBody)
      pure $ TLam $ TermLam ref body'
    (ETup (ExprTup left right), TSigma (TermSigma (T.Param tRef tTyp) tBody)) -> do
      left' <- checkExpr left tTyp
      right' <- checkExpr right (subst tRef left' tBody)
      pure $ TTup $ TermTup left' right'
    _ -> do
      (term, ty'') <- inferExpr expr
      unified <- unify (TUniv TermUniv) ty'' ty'
      unless unified $ throwError $ TEInferDiff expr ty'' ty'
      pure term

inferExpr :: TyckEff m => Expr Ref -> Eff m (Term, Term)
inferExpr expr = case expr of
  EApp (ExprApp fn (E.Arg arg)) -> do
    (fn', fnTy) <- inferExpr fn
    fnTy' <- normalizeToWhnf fnTy
    case fnTy' of
      TPi (TermPi (T.Param tRef tTyp) tBody) -> do
        arg' <- checkExpr arg tTyp
        pure (TApp $ TermApp fn' (T.Arg arg'), subst tRef arg' tBody)
      _ -> throwError $ TEAppFnNotPi expr fnTy'
  EPi (ExprPi (E.Param ref typ) body) -> do
    (typ', typTy) <- inferExpr typ
    typTy' <- normalizeToWhnf typTy
    unless (typTy' == TUniv TermUniv) $
      throwError $ TEParNotUniv expr typTy'
    withLocal ref typ' do
      (body', bodyTy) <- inferExpr body
      bodyTy' <- normalizeToWhnf bodyTy
      unless (bodyTy' == TUniv TermUniv) $
        throwError $ TEBodyNotUniv expr bodyTy'
      pure (TPi $ TermPi (T.Param ref typ') body', TUniv TermUniv)
  ETup (ExprTup left right) -> do
    (left', leftTy) <- inferExpr left
    (right', rightTy) <- inferExpr right
    dummy <- freshBind "_"
    pure (TTup $ TermTup left' right', TSigma $ TermSigma (T.Param dummy leftTy) rightTy)
  ESigma (ExprSigma (E.Param ref typ) body) -> do
    (typ', typTy) <- inferExpr typ
    typTy' <- normalizeToWhnf typTy
    unless (typTy' == TUniv TermUniv) $
      throwError $ TEParNotUniv expr typTy'
    withLocal ref typ' do
      (body', bodyTy) <- inferExpr body
      bodyTy' <- normalizeToWhnf bodyTy
      unless (bodyTy' == TUniv TermUniv) $
        throwError $ TEBodyNotUniv expr bodyTy'
      pure (TSigma $ TermSigma (T.Param ref typ') body', TUniv TermUniv)
  EProj (ExprProj tup isLeft) -> do
    (tup', tupTy) <- inferExpr tup
    tupTy' <- normalizeToWhnf tupTy
    case tupTy' of
      TSigma (TermSigma (T.Param tRef tTyp) tBody) -> if isLeft
        then pure (TProj $ TermProj tup' True, tTyp)
        else pure (TProj $ TermProj tup' False, subst tRef (TProj $ TermProj tup' True) tBody)
      _ -> throwError $ TEProjTupNotSigma expr tupTy'
  EUniv ExprUniv -> pure (TUniv TermUniv, TUniv TermUniv)
  EVar (ExprVar (RBind bnd)) -> do
    bndTy <- getLocal bnd
    pure (TBind $ TermBind bnd, bndTy)
  EVar (ExprVar (RGlobal (RGVar var))) -> do
    varDef <- getCoreVar var
    pure (TCallVar $ TermCallVar var, dTyp varDef)
  _ -> throwError $ TENonInferrable expr

checkHead :: TyckEff m => Stmt Ref -> Eff m Def
checkHead = \case
  SVar (StmtVar refVar typ _) -> do
    typ' <- checkExpr typ (TUniv TermUniv)
    let core = DefVar refVar typ' Nothing
    setCoreVar refVar core
    pure (DVar core)

checkBody :: TyckEff m => Stmt Ref -> Eff m Def
checkBody = \case
  SVar (StmtVar refVar _ body) -> do
    DefVar _ typ _ <- getCoreVar refVar
    body' <- checkExpr body typ
    let core = DefVar refVar typ (Just body')
    setCoreVar refVar core
    pure (DVar core)
