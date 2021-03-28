module Hina.Core where

import           Hina.Ref (BindId, Ref, VarId)

data Term
  = TApp TermApp
  | TLam TermLam
  | TPi TermPi
  | TTup TermTup
  | TSigma TermSigma
  | TProj TermProj
  | TUniv TermUniv
  | TBind TermBind
  | TCallVar TermCallVar
  deriving (Eq)

instance Show Term where
  show term = case term of
    TApp (TermApp f x)       -> "(" ++ show f ++ " " ++ show x ++ ")"
    TLam (TermLam p x)       -> "(" ++ show p ++ " => " ++ show x ++ ")"
    TPi (TermPi p x)         -> "(" ++ show p ++ " -> " ++ show x ++ ")"
    TTup (TermTup x y)       -> "(" ++ show x ++ ", " ++ show y ++ ")"
    TSigma (TermSigma p x)   -> "(" ++ show p ++ " ** " ++ show x ++ ")"
    TProj (TermProj t l)     -> show t ++ "." ++ if l then "0" else "1"
    TUniv TermUniv           -> "U"
    TBind (TermBind r)       -> show r
    TCallVar (TermCallVar r) -> show r

data TermApp = TermApp { tFn :: Term, tArg :: Arg }
  deriving (Eq)
data TermLam = TermLam { tPar :: BindId Ref, tBody :: Term }
  deriving (Eq)
data TermPi = TermPi { tPar :: Param, tBody :: Term }
  deriving (Eq)
data TermTup = TermTup { tLeft :: Term, tRight :: Term }
  deriving (Eq)
data TermSigma = TermSigma { tPar :: Param, tBody :: Term }
  deriving (Eq)
data TermProj = TermProj { tTup :: Term, tIsLeft :: Bool }
  deriving (Eq)
data TermUniv = TermUniv {}
  deriving (Eq)
data TermBind = TermBind { tRefBind :: BindId Ref }
  deriving (Eq)
data TermCallVar = TermCallVar { tRefVar :: VarId Ref }
  deriving (Eq)

data Param = Param { pRef :: BindId Ref, pTyp :: Term }
  deriving (Eq)

instance Show Param where
  show (Param r t) = "(" ++ show r ++ " : " ++ show t ++ ")"

data Arg = Arg { aTerm :: Term }
  deriving (Eq)

instance Show Arg where
  show (Arg x) = show x

data Def
  = DVar DefVar

instance Show Def where
  show def = case def of
    DVar (DefVar ref typ body) -> "var " ++ show ref ++ " : " ++ show typ ++ " => " ++ show body ++ ";\n"

data DefVar = DefVar { dRef :: VarId Ref, dTyp :: Term, dBody :: Maybe Term }
