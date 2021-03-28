module Hina where

import           Control.Monad.Freer       (Eff, Members, run)
import           Control.Monad.Freer.Error (Error, runError, throwError)
import           Control.Monad.Freer.Fresh (evalFresh)
import           Control.Monad.Freer.State (State, evalState)
import qualified Data.HashMap.Strict       as Map
import qualified Data.IntMap.Strict        as IntMap
import qualified Data.Text                 as T
import qualified Hina.Core                 as Core
import           Hina.Parse                (Token, prog, tokenize)
import           Hina.Ref                  (FreshEff)
import           Hina.Resolve              (resolveAll)
import           Hina.Resolve.Context      (ResolveContext (ResolveContext),
                                            ResolveError)
import           Hina.Tyck                 (checkBody, checkHead)
import           Hina.Tyck.Context         (CoreMapping (CoreMapping),
                                            LocalCtx (LocalCtx), TyckError)
import           Text.Earley               (Report, fullParses, parser)

data HinaError
  = HEParse (Report T.Text [Token])
  | HEResolve ResolveError
  | HETyck TyckError
  deriving (Show)

type HinaEff m = (Members '[Error HinaError, State CoreMapping] m, FreshEff m)

hina :: HinaEff m => T.Text -> Eff m [Core.Def]
hina txt = do
  let tokens = tokenize (T.unpack txt)
  ast <- case fullParses (parser prog) tokens of
    ([x], _) -> pure x
    (_, r)   -> throwError $ HEParse r
  resolved <- runError $ evalState (ResolveContext Map.empty Map.empty) $ resolveAll ast
  resolved' <- case resolved of
    Left e  -> throwError (HEResolve e)
    Right x -> pure x
  headChecked <- runError $ evalState (LocalCtx IntMap.empty) $ traverse checkHead resolved'
  case headChecked of
    Left e  -> throwError (HETyck e)
    Right _ -> pure ()
  bodyChecked <- runError $ evalState (LocalCtx IntMap.empty) $ traverse checkBody resolved'
  case bodyChecked of
    Left e  -> throwError (HETyck e)
    Right x -> pure x

runHina :: T.Text -> Either HinaError [Core.Def]
runHina = run . runError . evalState (CoreMapping IntMap.empty) . evalFresh 0 . hina
