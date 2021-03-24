module Hina.Mapping where

import           Control.Monad.Freer       (Eff, Member)
import           Control.Monad.Freer.State (State, get, modify)
import qualified Data.Map.Strict           as Map
import           Hina.Concrete             (StmtVar)
import           Hina.Ref                  (Ref, RefVar)

data ConcreteMapping = ConcreteMapping
  { cmVars :: Map.Map RefVar (StmtVar Ref) }

setCmVar :: Member (State ConcreteMapping) m => RefVar -> StmtVar Ref -> Eff m ()
setCmVar ref stmt = modify \cm -> cm { cmVars = Map.insert ref stmt (cmVars cm) }

getCmVar :: Member (State ConcreteMapping) m => RefVar -> Eff m (StmtVar Ref)
getCmVar ref = do
  mapping <- get
  pure (cmVars mapping Map.! ref)
