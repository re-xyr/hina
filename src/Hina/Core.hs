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

data Arg = Arg { aTerm :: Term }
  deriving (Eq)

data Def
  = DVar DefVar

data DefVar = DefVar { dRef :: VarId Ref, dTyp :: Term, dBody :: Maybe Term }
