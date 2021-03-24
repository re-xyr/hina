module Hina.Ref where

import           Control.Monad.Freer       (Eff, Member)
import           Control.Monad.Freer.Fresh (Fresh, fresh)
import qualified Data.HashMap.Strict       as Map
import qualified Data.Text                 as T

data Ref
  = RBind RefBind
  | RVar RefVar
  deriving (Show, Ord, Eq)

data RefBind = RefBind { rName :: Name, rUid :: Uid }
  deriving (Show, Ord, Eq)
data RefVar = RefVar { rName :: Name, rUid :: Uid }
  deriving (Show, Ord, Eq)

type Name = T.Text
type Uid = Int

type family BindId a where
  BindId Ref = RefBind
  BindId Name = Name

type family VarId a where
  VarId Ref = RefVar
  VarId Name = Name

freshBind :: Member Fresh m => Name -> Eff m RefBind
freshBind nm = RefBind nm <$> fresh

freshVar :: Member Fresh m => Name -> Eff m RefVar
freshVar nm = RefVar nm <$> fresh
