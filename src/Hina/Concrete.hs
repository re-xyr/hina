module Hina.Concrete where

import           Hina.Ref (Name, Ref, RefBind, RefVar)

type family BindId a where
  BindId Ref = RefBind
  BindId Name = Name

type family VarId a where
  VarId Ref = RefVar
  VarId Name = Name

data Expr id
  = EApp (ExprApp id)
  | ELam (ExprLam id)
  | EPi (ExprPi id)
  | ETup (ExprTup id)
  | ESigma (ExprSigma id)
  | EProj (ExprProj id)
  | EUniv (ExprUniv id)
  | EVar (ExprVar id)

data ExprApp id = ExprApp { eFn :: Expr id, eArg :: Arg id }
data ExprLam id = ExprLam { ePar :: Param id, eBody :: Expr id }
data ExprPi id = ExprPi { ePar :: Param id, eBody :: Expr id }
data ExprTup id = ExprTup { eLeft :: Expr id, eRight :: Expr id }
data ExprSigma id = ExprSigma { ePar :: Param id, eBody :: Expr id }
data ExprProj id = ExprProj { eTup :: Expr id, eIsLeft :: Bool }
data ExprUniv id = ExprUniv {}
data ExprVar id = ExprVar { eRef :: id }

data Param id = Param { pRef :: BindId id, pTyp :: Expr id }

data Arg id = Arg { aExpr :: Expr id }

data Stmt id
  = SVar (StmtVar id)

data StmtVar id = StmtVar { sRef :: VarId id, sTyp :: Expr id, sBody :: Expr id }
