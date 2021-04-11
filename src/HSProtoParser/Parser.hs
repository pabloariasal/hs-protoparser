{-# LANGUAGE OverloadedStrings #-}

module HSProtoParser.Parser
  ( parseProto,
  )
where

import Data.Text (Text, pack, unpack)
import Data.Void
import HSProtoParser.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

-- verbatim strings
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- generic lexemes
-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme sc

betweenChar :: Char -> Parser Text -> Parser Text
betweenChar c = between (char c) (char c)

parseSyntax :: Parser Text
parseSyntax = do
  _ <- sc
  _ <- symbol "syntax"
  _ <- symbol "="
  s <- betweenChar '"' (symbol "proto3") <|> betweenChar '\'' (symbol "proto3")
  _ <- symbol ";"
  return s

parseProto :: Parser ProtoFile
parseProto = do
  syntax <- parseSyntax
  return (ProtoFile (unpack syntax) [])
