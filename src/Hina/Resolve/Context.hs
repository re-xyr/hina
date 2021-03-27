module Hina.Resolve.Context where

import           Control.Monad             (when)
import           Control.Monad.Freer       (Eff, Member, Members)
import           Control.Monad.Freer.Error (Error, throwError)
import           Control.Monad.Freer.State (State, get, put)
import qualified Data.HashMap.Strict       as Map
import           Data.Maybe                (isJust)
import           Hina.Mapping              (ConcreteMapping)
import           Hina.Ref                  (FreshEff, Name,
                                            Ref (RBind, RGlobal), RefBind,
                                            RefGlobal)

data ResolveContext = ResolveContext
  { rcLocals  :: Map.HashMap Name [RefBind]
  , rcGlobals :: Map.HashMap Name RefGlobal }

type ResolveEff m = (Members '[State ResolveContext, Error ()] m, FreshEff m)

type ResolveGlobalEff m = (Member (State ConcreteMapping) m, ResolveEff m)

getUnqualifiedLocallyMaybe :: ResolveEff m => Name -> Eff m (Maybe RefBind)
getUnqualifiedLocallyMaybe nm = do
  ctx <- get
  pure case Map.lookup nm (rcLocals ctx) of
    Nothing      -> Nothing
    Just []      -> error "Impossible"
    Just (x : _) -> Just x

getUnqualifiedMaybe :: ResolveEff m => Name -> Eff m (Maybe Ref)
getUnqualifiedMaybe nm = do
  ctx <- get
  refMaybe <- getUnqualifiedLocallyMaybe nm
  pure case refMaybe of
    Nothing  -> RGlobal <$> Map.lookup nm (rcGlobals ctx)
    Just ref -> Just $ RBind ref

getUnqualified :: ResolveEff m => Name -> Eff m Ref
getUnqualified nm = do
  refMaybe <- getUnqualifiedMaybe nm
  case refMaybe of
    Nothing  -> throwError ()
    Just ref -> pure ref

addLocal :: ResolveEff m => Name -> RefBind -> Eff m ()
addLocal nm ref = do
  ctx <- get
  put ctx { rcLocals = Map.alter (\case
    Nothing -> Just [ref]
    Just xs -> Just (ref : xs)) nm (rcLocals ctx) }

removeLocal :: ResolveEff m => Name -> Eff m ()
removeLocal nm = do
  ctx <- get
  put ctx { rcLocals = Map.alter (\case
    Nothing       -> error "Impossible"
    Just []       -> error "Impossible"
    Just (_ : xs) -> Just xs) nm (rcLocals ctx) }

withLocal :: ResolveEff m => Name -> RefBind -> Eff m a -> Eff m a
withLocal nm ref m = do
  addLocal nm ref
  res <- m
  removeLocal nm
  pure res

addGlobal :: ResolveGlobalEff m => Name -> RefGlobal -> Eff m ()
addGlobal nm ref = do
  ctx <- get
  when (isJust (Map.lookup nm (rcGlobals ctx))) (throwError ())
  put ctx { rcGlobals = Map.insert nm ref (rcGlobals ctx) }

getGlobal :: ResolveGlobalEff m => Name -> Eff m RefGlobal
getGlobal nm = do
  ctx <- get
  pure $ rcGlobals ctx Map.! nm
