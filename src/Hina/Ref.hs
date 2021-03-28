module Hina.Ref where

import           Control.Monad.Freer       (Eff, Member)
import           Control.Monad.Freer.Fresh (Fresh, fresh)
import qualified Data.Text                 as T

data Ref
  = RBind RefBind
  | RGlobal RefGlobal
  deriving (Eq)

instance Show Ref where
  show = \case
    RBind x   -> show x
    RGlobal x -> show x

type FreshEff m = Member Fresh m

data RefBind = RefBind { rName :: Name, rUid :: Uid }
  deriving (Eq)

instance Show RefBind where
  show (RefBind name uid) = T.unpack name ++ "[" ++ show uid ++ "]"

data RefGlobal =
  RGVar RefGlobalVar
  deriving (Eq)

instance Show RefGlobal where
  show = \case
    RGVar x -> show x

data RefGlobalVar = RefGlobalVar { rName :: Name, rUid :: Uid }
  deriving (Eq)

instance Show RefGlobalVar where
  show (RefGlobalVar name _) = T.unpack name

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
