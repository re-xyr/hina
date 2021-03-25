module Hina.Core.Substitute where

import qualified Data.IntMap.Strict as Map
import           Hina.Core          (Arg (Arg), Param (Param),
                                     Term (TApp, TBind, TCallVar, TLam, TPi, TProj, TSigma, TTup, TUniv),
                                     TermApp (TermApp), TermBind (TermBind),
                                     TermCallVar (TermCallVar),
                                     TermLam (TermLam), TermPi (TermPi),
                                     TermProj (TermProj), TermSigma (TermSigma),
                                     TermTup (TermTup), TermUniv (TermUniv))
import           Hina.Ref           (RefBind (rUid))

substAll :: Map.IntMap Term -> Term -> Term
substAll mapping term = case term of
  TApp (TermApp fn arg) ->
    TApp (TermApp (substAll mapping fn) (substAllArg mapping arg))
  TLam (TermLam param body) ->
    TLam (TermLam (substAllParam mapping param) (substAll mapping body))
  TPi (TermPi param body) ->
    TPi (TermPi (substAllParam mapping param) (substAll mapping body))
  TTup (TermTup left right) ->
    TTup (TermTup (substAll mapping left) (substAll mapping right))
  TSigma (TermSigma param body) ->
    TSigma (TermSigma (substAllParam mapping param) (substAll mapping body))
  TProj (TermProj tup left) ->
    TProj (TermProj (substAll mapping tup) left)
  TUniv TermUniv ->
    TUniv TermUniv
  TBind (TermBind ref) -> case Map.lookup (rUid ref) mapping of
    Just x  -> x
    Nothing -> TBind (TermBind ref)
  TCallVar (TermCallVar ref) ->
    TCallVar (TermCallVar ref)

substAllArg :: Map.IntMap Term -> Arg -> Arg
substAllArg mapping (Arg term) = Arg (substAll mapping term)

substAllParam :: Map.IntMap Term -> Param -> Param
substAllParam mapping (Param bind typ) = Param bind (substAll mapping typ)

subst :: RefBind -> Term -> Term -> Term
subst r t = substAll (Map.singleton (rUid r) t)
