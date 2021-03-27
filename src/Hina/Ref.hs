module Hina.Ref where

import           Control.Monad.Freer       (Eff, Member)
import           Control.Monad.Freer.Fresh (Fresh, fresh)
import qualified Data.Text                 as T

data Ref
  = RBind RefBind
  | RGlobal RefGlobal
  deriving (Show, Ord, Eq)

type FreshEff m = Member Fresh m

data RefBind = RefBind { rName :: Name, rUid :: Uid }
  deriving (Show, Ord, Eq)

data RefGlobal =
  RGVar RefGlobalVar
  deriving (Show, Ord, Eq)

data RefGlobalVar = RefGlobalVar { rName :: Name, rUid :: Uid }
  deriving (Show, Ord, Eq)

type Name = T.Text
type Uid = Int

type family BindId a where
  BindId Ref = RefBind
  BindId Name = Name

type family VarId a where
  VarId Ref = RefGlobalVar
  VarId Name = Name

freshBind :: FreshEff m => Name -> Eff m RefBind
freshBind nm = RefBind nm <$> fresh

freshVar :: FreshEff m => Name -> Eff m RefGlobalVar
freshVar nm = RefGlobalVar nm <$> fresh
