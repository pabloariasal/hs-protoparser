{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HSProtoParser.Parser
  ( protoParser,
    parseProto,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import HSProtoParser.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- import Text.Megaparsec.Debug

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

stringLiteral :: Parser Text
stringLiteral = do
  l <-
    char '\'' *> manyTill L.charLiteral (char '\'')
      <|> char '"' *> manyTill L.charLiteral (char '"')
  _ <- sc
  return $ T.pack l

ident :: Parser Text
ident = do
  a <- T.singleton <$> alphaNumChar
  r <- T.pack <$> some (alphaNumChar <|> char '_')
  return (a `T.append` r)

fullIdent :: Parser Text
fullIdent = T.intercalate (T.singleton '.') <$> (ident `sepBy1` char '.')

parseSyntax :: Parser SyntaxDefinition
parseSyntax = do
  _ <- sc
  _ <- symbol "syntax"
  _ <- symbol "="
  s <- betweenChar '"' (symbol "proto3") <|> betweenChar '\'' (symbol "proto3")
  _ <- symbol ";"
  return (T.unpack s)

parsePackageDefinition :: Parser PackageDefinition
parsePackageDefinition = do
  _ <- symbol "package"
  p <- fullIdent
  _ <- symbol ";"
  return $ T.unpack p

parseImport :: Parser ImportStatement
parseImport = do
  _ <- symbol "import"
  access <- optional . try $ (Weak <$ symbol "weak" <|> Public <$ symbol "public")
  path <- stringLiteral
  _ <- symbol ";"
  return $ ImportStatement access (T.unpack path)

partitionTopLevelStatements :: [TopLevelStatement] -> ([ImportStatement], [PackageDefinition])
partitionTopLevelStatements = foldr acc initVal
  where
    initVal = ([], [])
    acc (ImportStmt e) ~(importSts, packages) = (e : importSts, packages)
    acc (PackageDef e) ~(importSts, packages) = (importSts, e : packages)

protoParser :: Parser ProtoFile
protoParser = do
  syntaxDef <- parseSyntax
  statements <- many $ choice [PackageDef <$> try parsePackageDefinition, ImportStmt <$> try parseImport]
  _ <- eof
  let (importStatements, packages) = partitionTopLevelStatements statements
  return (ProtoFile syntaxDef packages importStatements)

parseProto :: String -> String -> Either String ProtoFile
parseProto f i = case parse protoParser f (T.pack i) of
  Left b -> Left $ errorBundlePretty b
  -- TODO check if there are multiple package definitions present
  Right p -> Right p
