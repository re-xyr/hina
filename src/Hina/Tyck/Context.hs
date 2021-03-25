module Hina.Tyck.Context where

import           Control.Monad.Freer        (Eff, Member, Members)
import           Control.Monad.Freer.Reader (Reader)
import           Control.Monad.Freer.State  (State, get, modify)
import qualified Data.Map.Strict            as Map
import           Hina.Core                  (Term)
import           Hina.Mapping               (CoreMapping)
import           Hina.Ref                   (FreshEff, RefBind)

type LocalCtx = Map.Map RefBind Term

type TyckEff m = (Members '[Reader CoreMapping, State LocalCtx] m, FreshEff m)

withLocal :: Member (State LocalCtx) m => RefBind -> Term -> Eff m a -> Eff m a
withLocal r t m = do
  modify (Map.insert r t)
  res <- m
  modify @LocalCtx (Map.delete r)
  pure res

getLocal :: Member (State LocalCtx) m => RefBind -> Eff m Term
getLocal r = do
  ctx <- get
  pure $ ctx Map.! r
