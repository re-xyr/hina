module Hina.Resolve.Context where

import           Control.Monad             (when)
import           Control.Monad.Freer       (Eff, Members)
import           Control.Monad.Freer.Error (Error, throwError)
import           Control.Monad.Freer.State (State, get, put)
import qualified Data.HashMap.Strict       as Map
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Maybe                (isJust)
import           Hina.Ref                  (FreshEff, Name,
                                            Ref (RBind, RGlobal), RefBind,
                                            RefGlobal)

data ResolveContext = ResolveContext
  { rcLocals  :: Map.HashMap Name (NonEmpty.NonEmpty RefBind)
  , rcGlobals :: Map.HashMap Name RefGlobal }

data ResolveError
  = REUndefined Name
  | REDuplicate Name
  deriving (Show, Eq)

type ResolveEff m = (Members '[State ResolveContext, Error ResolveError] m, FreshEff m)

getUnqualifiedLocallyMaybe :: ResolveEff m => Name -> Eff m (Maybe RefBind)
getUnqualifiedLocallyMaybe nm = do
  ctx <- get
  pure case Map.lookup nm (rcLocals ctx) of
    Nothing                -> Nothing
    Just (x NonEmpty.:| _) -> Just x

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
    Nothing  -> throwError $ REUndefined nm
    Just ref -> pure ref

addLocal :: ResolveEff m => Name -> RefBind -> Eff m ()
addLocal nm ref = do
  ctx <- get
  put ctx { rcLocals = Map.alter (\case
    Nothing -> Just (ref NonEmpty.:| [])
    Just xs -> Just (ref `NonEmpty.cons` xs)) nm (rcLocals ctx) }

removeLocal :: ResolveEff m => Name -> Eff m ()
removeLocal nm = do
  ctx <- get
  put ctx { rcLocals = Map.alter (\case
    Nothing                       -> error "Impossible"
    Just (_ NonEmpty.:| [])       -> Nothing
    Just (_ NonEmpty.:| (x : xs)) -> Just $ x NonEmpty.:| xs) nm (rcLocals ctx) }

withLocal :: ResolveEff m => Name -> RefBind -> Eff m a -> Eff m a
withLocal nm ref m = do
  addLocal nm ref
  res <- m
  removeLocal nm
  pure res

addGlobal :: ResolveEff m => Name -> RefGlobal -> Eff m ()
addGlobal nm ref = do
  ctx <- get
  when (isJust (Map.lookup nm (rcGlobals ctx))) (throwError $ REDuplicate nm)
  put ctx { rcGlobals = Map.insert nm ref (rcGlobals ctx) }

getGlobal :: ResolveEff m => Name -> Eff m RefGlobal
getGlobal nm = do
  ctx <- get
  pure $ rcGlobals ctx Map.! nm
