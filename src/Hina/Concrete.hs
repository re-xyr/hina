module Hina.Concrete where

import qualified Data.Text as T
import           Hina.Ref  (BindId, Name, Ref, VarId)

data Expr id
  = EApp (ExprApp id)
  | ELam (ExprLam id)
  | EPi (ExprPi id)
  | ETup (ExprTup id)
  | ESigma (ExprSigma id)
  | EProj (ExprProj id)
  | EUniv (ExprUniv id)
  | EVar (ExprVar id)
deriving instance Eq (Expr Name)
deriving instance Eq (Expr Ref)

instance Show (Expr Name) where
  show = \case
    EApp (ExprApp f x)     -> "(" ++ show f ++ " " ++ show x ++ ")"
    ELam (ExprLam p x)     -> "(" ++ T.unpack p ++ " => " ++ show x ++ ")"
    EPi (ExprPi p x)       -> "(" ++ show p ++ " -> " ++ show x ++ ")"
    ETup (ExprTup x y)     -> "(" ++ show x ++ ", " ++ show y ++ ")"
    ESigma (ExprSigma p x) -> "(" ++ show p ++ " ** " ++ show x ++ ")"
    EProj (ExprProj t l)   -> show t ++ "." ++ if l then "0" else "1"
    EUniv ExprUniv         -> "U"
    EVar (ExprVar r)       -> T.unpack r

instance Show (Expr Ref) where
  show = \case
    EApp (ExprApp f x)     -> "(" ++ show f ++ " " ++ show x ++ ")"
    ELam (ExprLam p x)     -> "(" ++ show p ++ " => " ++ show x ++ ")"
    EPi (ExprPi p x)       -> "(" ++ show p ++ " -> " ++ show x ++ ")"
    ETup (ExprTup x y)     -> "(" ++ show x ++ ", " ++ show y ++ ")"
    ESigma (ExprSigma p x) -> "(" ++ show p ++ " ** " ++ show x ++ ")"
    EProj (ExprProj t l)   -> show t ++ "." ++ if l then "0" else "1"
    EUniv ExprUniv         -> "U"
    EVar (ExprVar r)       -> show r

data ExprApp id = ExprApp { eFn :: Expr id, eArg :: Arg id }
deriving instance Eq (ExprApp Name)
deriving instance Eq (ExprApp Ref)

data ExprLam id = ExprLam { ePar :: BindId id, eBody :: Expr id }
deriving instance Eq (ExprLam Name)
deriving instance Eq (ExprLam Ref)

data ExprPi id = ExprPi { ePar :: Param id, eBody :: Expr id }
deriving instance Eq (ExprPi Name)
deriving instance Eq (ExprPi Ref)

data ExprTup id = ExprTup { eLeft :: Expr id, eRight :: Expr id }
deriving instance Eq (ExprTup Name)
deriving instance Eq (ExprTup Ref)

data ExprSigma id = ExprSigma { ePar :: Param id, eBody :: Expr id }
deriving instance Eq (ExprSigma Name)
deriving instance Eq (ExprSigma Ref)

data ExprProj id = ExprProj { eTup :: Expr id, eIsLeft :: Bool }
deriving instance Eq (ExprProj Name)
deriving instance Eq (ExprProj Ref)

data ExprUniv id = ExprUniv {}
deriving instance Eq (ExprUniv Name)
deriving instance Eq (ExprUniv Ref)

data ExprVar id = ExprVar { eRef :: id }
deriving instance Eq (ExprVar Name)
deriving instance Eq (ExprVar Ref)

data Param id = Param { pRef :: BindId id, pTyp :: Expr id }
deriving instance Eq (Param Name)
deriving instance Eq (Param Ref)

instance Show (Param Name) where
  show (Param ref typ) = "(" ++ T.unpack ref ++ " : " ++ show typ ++ ")"

instance Show (Param Ref) where
  show (Param ref typ) = "(" ++ show ref ++ " : " ++ show typ ++ ")"

data Arg id = Arg { aExpr :: Expr id }
deriving instance Eq (Arg Name)
deriving instance Eq (Arg Ref)

instance Show (Arg Name) where
  show (Arg ex) = show ex

instance Show (Arg Ref) where
  show (Arg ex) = show ex

data Stmt id
  = SVar (StmtVar id)
deriving instance Eq (Stmt Name)
deriving instance Eq (Stmt Ref)

instance Show (Stmt Name) where
  show def = case def of
    SVar (StmtVar ref typ body) -> "var " ++ T.unpack ref ++ " : " ++ show typ ++ " => " ++ show body ++ ";\n"

instance Show (Stmt Ref) where
  show def = case def of
    SVar (StmtVar ref typ body) -> "var " ++ show ref ++ " : " ++ show typ ++ " => " ++ show body ++ ";\n"

data StmtVar id = StmtVar { sRef :: VarId id, sTyp :: Expr id, sBody :: Expr id }
deriving instance Eq (StmtVar Name)
deriving instance Eq (StmtVar Ref)
