module Hina.Tyck.Context where

import           Control.Monad.Freer       (Eff, Member, Members)
import           Control.Monad.Freer.Error (Error)
import           Control.Monad.Freer.State (State, get, modify)
import qualified Data.IntMap.Strict        as Map
import           Hina.Concrete             (Expr)
import           Hina.Core                 (DefVar, Term)
import           Hina.Ref                  (FreshEff, Ref,
                                            RefBind (RefBind, rUid),
                                            RefGlobalVar (RefGlobalVar, rUid))

data CoreMapping = CoreMapping
  { coreVars :: Map.IntMap DefVar }

setCoreVar :: Member (State CoreMapping) m => RefGlobalVar -> DefVar -> Eff m ()
setCoreVar RefGlobalVar {rUid} def = modify \cm -> cm { coreVars = Map.insert rUid def (coreVars cm) }

getCoreVar :: Member (State CoreMapping) m => RefGlobalVar -> Eff m DefVar
getCoreVar RefGlobalVar {rUid} = do
  mapping <- get
  pure (coreVars mapping Map.! rUid)

newtype LocalCtx = LocalCtx { unLocalCtx :: Map.IntMap Term }

data TyckError
  = TEInferDiff { expr :: Expr Ref, inferredType :: Term, checkedAgainst :: Term }
  | TEAppFnNotPi { appExpr :: Expr Ref, fnType :: Term }
  | TEProjTupNotSigma { projExpr :: Expr Ref, tupType :: Term }
  | TEParNotUniv { expr :: Expr Ref,  parType :: Term }
  | TEBodyNotUniv { expr :: Expr Ref, bodyType :: Term }
  | TENonInferrable (Expr Ref)
  deriving (Show)

type TyckEff m = (Members '[State CoreMapping, State LocalCtx, Error TyckError] m, FreshEff m)

withLocal :: Member (State LocalCtx) m => RefBind -> Term -> Eff m a -> Eff m a
withLocal RefBind {rUid} t m = do
  modify (LocalCtx . Map.insert rUid t . unLocalCtx)
  res <- m
  modify (LocalCtx . Map.delete rUid . unLocalCtx)
  pure res

getLocal :: Member (State LocalCtx) m => RefBind -> Eff m Term
getLocal RefBind {rUid} = do
  ctx <- unLocalCtx <$> get
  pure $ ctx Map.! rUid
