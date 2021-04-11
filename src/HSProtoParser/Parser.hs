{-# LANGUAGE OverloadedStrings #-}

module HSProtoParser.Parser
  ( parseProto,
    parsePackage,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import HSProtoParser.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

-- verbatim strings, consuming trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- generic lexemes, consuming trailing whitespace
-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme sc

betweenChar :: Char -> Parser Text -> Parser Text
betweenChar c = between (char c) (char c)

ident :: Parser Text
ident = do
  a <- T.singleton <$> alphaNumChar
  r <- T.pack <$> some (alphaNumChar <|> char '_')
  return (a `T.append` r)

fullIdent :: Parser Text
fullIdent = T.intercalate (T.singleton '.') <$> (ident `sepBy1` char '.')

parseSyntax :: Parser Text
parseSyntax = do
  _ <- sc
  _ <- symbol "syntax"
  _ <- symbol "="
  s <- betweenChar '"' (symbol "proto3") <|> betweenChar '\'' (symbol "proto3")
  _ <- symbol ";"
  return s

parsePackage :: Parser Text
parsePackage = do
  _ <- symbol "package"
  p <- fullIdent
  _ <- symbol ";"
  return p

parseProto :: Parser ProtoFile
parseProto = do
  syntax <- parseSyntax
  package <- optional . try $ parsePackage
  return (ProtoFile (T.unpack syntax) (T.unpack <$> package))
