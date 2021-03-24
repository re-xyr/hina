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

data TermApp = TermApp { tFn :: Term, tArg :: Arg }
data TermLam = TermLam { tPar :: Param, tBody :: Term }
data TermPi = TermPi { tPar :: Param, tBody :: Term }
data TermTup = TermTup { tLeft :: Term, tRight :: Term }
data TermSigma = TermSigma { tPar :: Param, tBody :: Term }
data TermProj = TermProj { tTup :: Term, tIsLeft :: Bool }
data TermUniv = TermUniv {}
data TermBind = TermBind { tRefBind :: BindId Ref }
data TermCallVar = TermCallVar { tRefVar :: VarId Ref }

data Param = Param { pRef :: BindId Ref, pTyp :: Term }

data Arg = Arg { aTerm :: Term }

data Def
  = DVar DefVar

data DefVar = DefVar { dRef :: VarId Ref, dTyp :: Term, dBody :: Maybe Term }
