module Hina.Resolve where

import           Control.Monad.Freer  (Eff)
import           Hina.Concrete        (Arg (Arg),
                                       Expr (EApp, ELam, EPi, EProj, ESigma, ETup, EUniv, EVar),
                                       ExprApp (ExprApp), ExprLam (ExprLam),
                                       ExprPi (ExprPi), ExprProj (ExprProj),
                                       ExprSigma (ExprSigma), ExprTup (ExprTup),
                                       ExprUniv (ExprUniv), ExprVar (ExprVar),
                                       Param (Param), Stmt (SVar),
                                       StmtVar (StmtVar))
import           Hina.Ref             (Name, Ref, RefGlobal (RGVar), freshBind,
                                       freshVar)
import           Hina.Resolve.Context (ResolveEff, addGlobal, getUnqualified,
                                       withLocal)

resolveExpr :: ResolveEff m => Expr Name -> Eff m (Expr Ref)
resolveExpr expr = case expr of
  EApp (ExprApp fn arg) -> do
    fn' <- resolveExpr fn
    arg' <- resolveArg arg
    pure $ EApp $ ExprApp fn' arg'
  ELam (ExprLam param body) -> do
    localRef <- freshBind param
    withLocal param localRef do
      body' <- resolveExpr body
      pure $ ELam $ ExprLam localRef body'
  EPi (ExprPi param body) ->
    resolveParam param \param' -> do
      body' <- resolveExpr body
      pure $ EPi $ ExprPi param' body'
  ETup (ExprTup left right) -> do
    left' <- resolveExpr left
    right' <- resolveExpr right
    pure $ ETup $ ExprTup left' right'
  ESigma (ExprSigma param body) ->
    resolveParam param \param' -> do
      body' <- resolveExpr body
      pure $ ESigma $ ExprSigma param' body'
  EProj (ExprProj tup left) -> do
    tup' <- resolveExpr tup
    pure $ EProj $ ExprProj tup' left
  EUniv ExprUniv ->
    pure $ EUniv ExprUniv
  EVar (ExprVar ref) -> do
    ref' <- getUnqualified ref
    pure $ EVar $ ExprVar ref'

resolveArg :: ResolveEff m => Arg Name -> Eff m (Arg Ref)
resolveArg (Arg expr) = do
  expr' <- resolveExpr expr
  pure $ Arg expr'

resolveParam :: ResolveEff m => Param Name -> (Param Ref -> Eff m a) -> Eff m a
resolveParam (Param ref typ) f = do
  localRef <- freshBind ref
  typ' <- resolveExpr typ
  withLocal ref localRef $ f (Param localRef typ')

resolveStmt :: ResolveEff m => Stmt Name -> Eff m (Eff m (Stmt Ref))
resolveStmt stmt = case stmt of
  SVar (StmtVar ref typ body) -> do
    varRef <- freshVar ref
    addGlobal ref (RGVar varRef)
    pure do
      typ' <- resolveExpr typ
      body' <- resolveExpr body
      let stmt' = StmtVar varRef typ' body'
      pure $ SVar stmt'

resolveAll :: ResolveEff m => [Stmt Name] -> Eff m [Stmt Ref]
resolveAll stmts = do
  preResolved <- traverse resolveStmt stmts
  sequence preResolved
