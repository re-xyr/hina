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
                                            ExprVar (ExprVar))
import qualified Hina.Concrete             as E
import           Hina.Core                 (Term (TApp, TBind, TCallVar, TLam, TPi, TProj, TSigma, TTup, TUniv),
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
import           Hina.Mapping              (askCoreVar)
import           Hina.Ref                  (Ref (RBind, RVar), freshBind)
import           Hina.Tyck.Context         (TyckEff, getLocal, withLocal)
import           Hina.Tyck.Unify           (unify)

checkExpr :: TyckEff m => Expr Ref -> Term -> Eff m Term
checkExpr expr ty = do
  ty' <- normalizeToWhnf ty
  case (expr, ty') of
    (ELam (ExprLam ref body), TPi (TermPi (T.Param tRef tTyp) tBody)) -> do
      body' <- checkExpr body (subst tRef (TBind $ TermBind ref) tBody)
      pure $ TLam $ TermLam ref body'
    (ETup (ExprTup left right), TSigma (TermSigma (T.Param tRef tTyp) tBody)) -> do
      left' <- checkExpr left tTyp
      right' <- checkExpr right (subst tRef left' tBody)
      pure $ TTup $ TermTup left' right'
    _ -> do
      (term, ty'') <- inferExpr expr
      unified <- unify (TUniv TermUniv) ty'' ty'
      unless unified $ throwError ()
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
      _ -> throwError ()
  EPi (ExprPi (E.Param ref typ) body) -> do
    (typ', typTy) <- inferExpr typ
    typTy' <- normalizeToWhnf typTy
    unless (typTy' == TUniv TermUniv) $ throwError ()
    withLocal ref typ' do
      (body', bodyTy) <- inferExpr body
      bodyTy' <- normalizeToWhnf bodyTy
      unless (bodyTy' == TUniv TermUniv) $ throwError ()
      pure (TPi $ TermPi (T.Param ref typ') body', TUniv TermUniv)
  ETup (ExprTup left right) -> do
    (left', leftTy) <- inferExpr left
    (right', rightTy) <- inferExpr right
    dummy <- freshBind "_"
    pure (TTup $ TermTup left' right', TSigma $ TermSigma (T.Param dummy leftTy) rightTy)
  ESigma (ExprSigma (E.Param ref typ) body) -> do
    (typ', typTy) <- inferExpr typ
    typTy' <- normalizeToWhnf typTy
    unless (typTy' == TUniv TermUniv) $ throwError ()
    withLocal ref typ' do
      (body', bodyTy) <- inferExpr body
      bodyTy' <- normalizeToWhnf bodyTy
      unless (bodyTy' == TUniv TermUniv) $ throwError ()
      pure (TSigma $ TermSigma (T.Param ref typ') body', TUniv TermUniv)
  EProj (ExprProj tup isLeft) -> do
    (tup', tupTy) <- inferExpr tup
    tupTy' <- normalizeToWhnf tupTy
    case tupTy' of
      TSigma (TermSigma (T.Param tRef tTyp) tBody) -> if isLeft
        then pure (TProj $ TermProj tup' True, tTyp)
        else pure (TProj $ TermProj tup' False, subst tRef (TProj $ TermProj tup' True) tBody)
  EUniv ExprUniv -> pure (TUniv TermUniv, TUniv TermUniv)
  EVar (ExprVar (RBind bnd)) -> do
    bndTy <- getLocal bnd
    pure (TBind $ TermBind bnd, bndTy)
  EVar (ExprVar (RVar var)) -> do
    varDef <- askCoreVar var
    pure (TCallVar $ TermCallVar var, dTyp varDef)
  _ -> throwError ()
