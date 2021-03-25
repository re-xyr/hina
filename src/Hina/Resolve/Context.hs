module Hina.Resolve.Context where

import           Control.Monad              (when)
import           Control.Monad.Freer        (Eff, Members)
import           Control.Monad.Freer.Error  (Error, throwError)
import           Control.Monad.Freer.Reader (Reader, ask, runReader)
import           Control.Monad.Freer.State  (State, get, put)
import           Control.Monad.Freer.TH     (makeEffect)
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe                 (isJust)
import           Hina.Mapping               (ConcreteMapping)
import           Hina.Ref                   (FreshEff, Name, Ref (RBind, RVar),
                                             RefBind, RefVar)

data ResolveContext
  = RCRoot RootContext
  | RCBind BindContext

data BindContext = BindContext
  { rcParent   :: ResolveContext
  , rcBindName :: Name
  , rcBindRef  :: RefBind }

data RootContext = RootContext
  { rcGlobals :: Map.HashMap Name RefVar }

type ResolveEff m = (Members '[Reader ResolveContext, Error ()] m, FreshEff m)

type ResolveGlobalEff m = (Members '[State RootContext, State ConcreteMapping, Error ()] m, FreshEff m)

getParent :: ResolveEff m => Eff m (Maybe ResolveContext)
getParent = do
  ctx <- ask
  case ctx of
    RCBind ctx -> pure $ Just $ rcParent ctx
    RCRoot ctx -> pure Nothing

getUnqualifiedLocallyMaybe :: ResolveEff m => Name -> Eff m (Maybe Ref)
getUnqualifiedLocallyMaybe nm = do
  ctx <- ask
  case ctx of
    RCBind ctx -> if nm == rcBindName ctx
      then pure $ Just $ RBind $ rcBindRef ctx
      else pure Nothing
    RCRoot ctx -> case Map.lookup nm (rcGlobals ctx) of
      Nothing    -> pure Nothing
      Just entry -> pure $ Just $ RVar entry

getUnqualifiedMaybe :: ResolveEff m => Name -> Eff m (Maybe Ref)
getUnqualifiedMaybe nm = do
  refMaybe <- getUnqualifiedLocallyMaybe nm
  prtMaybe <- getParent
  case refMaybe of
    Nothing -> case prtMaybe of
      Nothing  -> pure Nothing
      Just prt -> runReader prt $ getUnqualifiedMaybe nm
    Just ref -> pure $ Just ref

getUnqualified :: ResolveEff m => Name -> Eff m Ref
getUnqualified nm = do
  refMaybe <- getUnqualifiedMaybe nm
  case refMaybe of
    Nothing  -> throwError ()
    Just ref -> pure ref

addGlobal :: ResolveGlobalEff m => Name -> RefVar -> Eff m ()
addGlobal nm ref = do
  ctx <- get
  when (isJust (Map.lookup nm (rcGlobals ctx))) (throwError ())
  put ctx { rcGlobals = Map.insert nm ref (rcGlobals ctx) }
