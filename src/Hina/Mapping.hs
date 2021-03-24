module Hina.Mapping where

import           Control.Monad.Freer       (Eff, Member)
import           Control.Monad.Freer.State (State, get, modify)
import qualified Data.Map.Strict           as Map
import           Hina.Concrete             (StmtVar)
import           Hina.Core                 (DefVar)
import           Hina.Ref                  (Ref, RefVar)

data ConcreteMapping = ConcreteMapping
  { concVars :: Map.Map RefVar (StmtVar Ref) }

setConcVar :: Member (State ConcreteMapping) m => RefVar -> StmtVar Ref -> Eff m ()
setConcVar ref stmt = modify \cm -> cm { concVars = Map.insert ref stmt (concVars cm) }

getConcVar :: Member (State ConcreteMapping) m => RefVar -> Eff m (StmtVar Ref)
getConcVar ref = do
  mapping <- get
  pure (concVars mapping Map.! ref)

data CoreMapping = CoreMapping
  { coreVars :: Map.Map RefVar DefVar }

setCoreVar :: Member (State CoreMapping) m => RefVar -> DefVar -> Eff m ()
setCoreVar ref def = modify \cm -> cm { coreVars = Map.insert ref def (coreVars cm) }

getCoreVar :: Member (State CoreMapping) m => RefVar -> Eff m DefVar
getCoreVar ref = do
  mapping <- get
  pure (coreVars mapping Map.! ref)
