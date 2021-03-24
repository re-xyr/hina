module Hina.Resolve where

import           Control.Monad.Freer        (Eff)
import           Control.Monad.Freer.Reader (ask, local, runReader)
import           Control.Monad.Freer.State  (get)
import qualified Data.HashMap.Strict        as Map
import           Data.Traversable           (for)
import           Hina.Concrete              (Arg (Arg),
                                             Expr (EApp, ELam, EPi, EProj, ESigma, ETup, EUniv, EVar),
                                             ExprApp (ExprApp),
                                             ExprLam (ExprLam), ExprPi (ExprPi),
                                             ExprProj (ExprProj),
                                             ExprSigma (ExprSigma),
                                             ExprTup (ExprTup),
                                             ExprUniv (ExprUniv),
                                             ExprVar (ExprVar), Param (Param),
                                             Stmt (SVar), StmtVar (StmtVar))
import           Hina.Mapping               (setConcVar)
import           Hina.Ref                   (Name, Ref, freshBind, freshVar)
import           Hina.Resolve.Context       (BindContext (BindContext),
                                             ResolveContext (RCBind, RCRoot),
                                             ResolveEff, ResolveGlobalEff,
                                             addGlobal, getUnqualified)

resolveExpr :: ResolveEff m => Expr Name -> Eff m (Expr Ref)
resolveExpr expr = case expr of
  EApp (ExprApp fn arg) -> do
    fn' <- resolveExpr fn
    arg' <- resolveArg arg
    pure $ EApp $ ExprApp fn' arg'
  ELam (ExprLam param body) ->
    resolveParam param \param' -> do
      body' <- resolveExpr body
      pure $ ELam $ ExprLam param' body'
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
  ctx <- ask
  localRef <- freshBind ref
  let boundCtx = BindContext ctx ref localRef
  typ' <- resolveExpr typ
  local (const $ RCBind boundCtx) $ f $ Param localRef typ'

resolveStmt :: ResolveGlobalEff m => Stmt Name -> Eff m (Stmt Ref)
resolveStmt stmt = case stmt of
  SVar (StmtVar ref typ body) -> do
    ctx <- get
    varRef <- freshVar ref
    typ' <- runReader (RCRoot ctx) $ resolveExpr typ
    addGlobal ref varRef
    ctx <- get
    body' <- runReader (RCRoot ctx) $ resolveExpr body
    let stmt' = StmtVar varRef typ' body'
    setConcVar varRef stmt'
    pure $ SVar stmt'
