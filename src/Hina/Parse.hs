module Hina.Parse where

import           Control.Applicative             (many, (<|>))
import           Control.Applicative.Combinators (between)
import           Data.Char                       (isSpace)
import qualified Data.Text                       as T
import           Hina.Concrete                   (Arg (Arg),
                                                  Expr (EApp, ELam, EPi, EProj, ESigma, ETup, EUniv, EVar),
                                                  ExprApp (ExprApp),
                                                  ExprLam (ExprLam),
                                                  ExprPi (ExprPi),
                                                  ExprProj (ExprProj),
                                                  ExprSigma (ExprSigma),
                                                  ExprTup (ExprTup),
                                                  ExprUniv (ExprUniv),
                                                  ExprVar (ExprVar),
                                                  Param (Param), Stmt (SVar),
                                                  StmtVar (StmtVar))
import           Hina.Ref                        (Name)
import           Text.Earley                     (Grammar, Prod, rule, terminal,
                                                  token)

data Token
  = TWord T.Text
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TSemicolon
  | TDot
  | TComma
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize str = case str of
  '(' : xs -> TLParen : tokenize xs
  ')' : xs -> TRParen : tokenize xs
  '{' : xs -> TLBrace : tokenize xs
  '}' : xs -> TRBrace : tokenize xs
  ';' : xs -> TSemicolon : tokenize xs
  '.' : xs -> TDot : tokenize xs
  ',' : xs -> TComma : tokenize xs
  x : xs
    | isSpace x -> tokenize xs
    | otherwise -> tokenizeWord [x] xs
  [] -> []
  where
    tokenizeWord w [] = [TWord $ T.pack $ reverse w]
    tokenizeWord w (x : xs)
      | isSpace x || x `elem` ("(){};.," :: String) = TWord (T.pack $ reverse w) : tokenize (x : xs)
      | otherwise = tokenizeWord (x : w) xs

keywords :: [T.Text]
keywords = ["=>", "->", ":", "**", "U"]

parens :: Prod r T.Text Token a -> Prod r T.Text Token a
parens = between (token TLParen) (token TRParen)

parseName :: Prod r T.Text Token Name
parseName = terminal \case
  TWord x -> if x `elem` keywords then Nothing else Just x
  _       -> Nothing

expr :: Grammar r (Prod r T.Text Token (Expr Name))
expr = mdo
  name <- rule parseName
  param <- rule $ parens $ Param <$> name <* token (TWord ":") <*> ex
  let ex = pi
  pi <- rule $ sigma
    <|> (\x y -> EPi (ExprPi x y))
    <$> param <* token (TWord "->") <*> ex
  sigma <- rule $ lam
    <|> (\x y -> ESigma (ExprSigma x y))
    <$> param <* token (TWord "**") <*> ex
  lam <- rule $ arr
    <|> (\x y -> ELam (ExprLam x y))
    <$> name <* token (TWord "=>") <*> ex
  arr <- rule $ app
    <|> (\x y -> EPi (ExprPi (Param "_" x) y))
    <$> app <* token (TWord "->") <*> arr
  app <- rule $ proj
    <|> (\x y -> EApp (ExprApp x (Arg y)))
    <$> app <*> proj
  proj <- rule $ tup
    <|> (\x y -> EProj (ExprProj x y))
    <$> proj <* token TDot <*> (True <$ token (TWord "0") <|> False <$ token (TWord "1"))
  tup <- rule $ var
    <|> parens ((\x y -> ETup (ExprTup x y))
    <$> ex <* token TComma <*> ex)
  var <- rule $ univ
    <|> EVar . ExprVar
    <$> name
  univ <- rule $ wrap
    <|> EUniv ExprUniv
    <$  token (TWord "U")
  wrap <- rule $ parens ex
  pure ex

stmt :: Grammar r (Prod r T.Text Token (Stmt Name))
stmt = mdo
  name <- rule parseName
  ex <- expr
  st <- rule $ var <* token TSemicolon
  var <- rule $ (\x y z -> SVar (StmtVar x y z))
    <$ token (TWord "var") <*> name
    <* token (TWord ":") <*> ex
    <* token (TWord "=>") <*> ex
  pure st

prog :: Grammar r (Prod r T.Text Token [Stmt Name])
prog = mdo
  st <- stmt
  pr <- rule $ many st
  pure pr
