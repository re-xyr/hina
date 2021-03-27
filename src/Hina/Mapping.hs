module Hina.Mapping where

import           Control.Monad.Freer        (Eff, Member)
import           Control.Monad.Freer.Reader (Reader, ask)
import           Control.Monad.Freer.State  (State, get, modify)
import qualified Data.IntMap.Strict         as Map
import           Hina.Concrete              (StmtVar)
import           Hina.Core                  (DefVar)
import           Hina.Ref                   (Ref, RefGlobalVar (rUid))

data ConcreteMapping = ConcreteMapping
  { concVars :: Map.IntMap (StmtVar Ref) }

setConcVar :: Member (State ConcreteMapping) m => RefGlobalVar -> StmtVar Ref -> Eff m ()
setConcVar ref stmt = modify \cm -> cm { concVars = Map.insert (rUid ref) stmt (concVars cm) }

getConcVar :: Member (State ConcreteMapping) m => RefGlobalVar -> Eff m (StmtVar Ref)
getConcVar ref = do
  mapping <- get
  pure (concVars mapping Map.! rUid ref)

askConcVar :: Member (Reader ConcreteMapping) m => RefGlobalVar -> Eff m (StmtVar Ref)
askConcVar ref = do
  mapping <- ask
  pure (concVars mapping Map.! rUid ref)

data CoreMapping = CoreMapping
  { coreVars :: Map.IntMap DefVar }

setCoreVar :: Member (State CoreMapping) m => RefGlobalVar -> DefVar -> Eff m ()
setCoreVar ref def = modify \cm -> cm { coreVars = Map.insert (rUid ref) def (coreVars cm) }

getCoreVar :: Member (State CoreMapping) m => RefGlobalVar -> Eff m DefVar
getCoreVar ref = do
  mapping <- get
  pure (coreVars mapping Map.! rUid ref)

askCoreVar :: Member (Reader CoreMapping) m => RefGlobalVar -> Eff m DefVar
askCoreVar ref = do
  mapping <- ask
  pure (coreVars mapping Map.! rUid ref)
