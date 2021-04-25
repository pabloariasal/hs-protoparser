{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HSProtoParser.Parser
  ( protoParser,
    parseProto,
  )
where

import Data.Scientific (toRealFloat)
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
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme (L.decimal <* notFollowedBy (char '.' <|> char 'e' <|> char 'E'))

signedInteger :: Parser Int
signedInteger = L.signed sc integer

float :: Parser Float
float = toRealFloat <$> lexeme L.scientific

signedFloat :: Parser Float
signedFloat = L.signed sc float

betweenChar :: Char -> Parser Text -> Parser Text
betweenChar c = between (char c) (char c)

stringLiteral :: Parser Text
stringLiteral = do
  l <-
    char '\'' *> manyTill L.charLiteral (char '\'')
      <|> char '"' *> manyTill L.charLiteral (char '"')
  _ <- sc
  return $ T.pack l

-- starts with a letter, followed by any combination of alphanumeric and '_'
ident :: Parser Text
ident = do
  a <- T.singleton <$> letterChar
  r <- T.pack <$> many (alphaNumChar <|> char '_')
  _ <- sc
  return (a `T.append` r)

boolLit :: Parser Bool
boolLit = True <$ symbol "true" <|> False <$ "false"

-- '.' separated idents
parseFullIdent :: Parser Text
parseFullIdent = T.intercalate (T.singleton '.') <$> (ident `sepBy1` char '.')

parseSyntax :: Parser SyntaxStatement
parseSyntax = do
  _ <- sc
  _ <- symbol "syntax"
  _ <- symbol "="
  s <- betweenChar '"' (symbol "proto3") <|> betweenChar '\'' (symbol "proto3")
  _ <- some $ symbol ";"
  return (T.unpack s)

parsePackageSpecification :: Parser PackageSpecification
parsePackageSpecification = do
  _ <- symbol "package"
  p <- parseFullIdent
  _ <- some $ symbol ";"
  return $ T.unpack p

parseImportStatement :: Parser ImportStatement
parseImportStatement = do
  _ <- symbol "import"
  access <- optional . try $ (Weak <$ symbol "weak" <|> Public <$ symbol "public")
  path <- stringLiteral
  _ <- some $ symbol ";"
  return $ ImportStatement access (T.unpack path)

parseConstant :: Parser Constant
parseConstant =
  choice
    [ BoolLit <$> try boolLit,
      StringLit . T.unpack <$> try stringLiteral,
      Identifier . T.unpack <$> try parseFullIdent,
      IntLit <$> try signedInteger,
      FloatLit <$> try signedFloat
    ]

parseOptionName :: Parser Text
parseOptionName = T.append <$> (simple <|> surrounded) <*> (T.concat <$> many suffix)
  where
    simple = ident
    surrounded = between (char '(') (char ')') parseFullIdent
    suffix = T.append <$> (T.singleton <$> char '.') <*> ident

parseOptionDefinition :: Parser OptionDefinition
parseOptionDefinition = do
  _ <- symbol "option"
  k <- parseOptionName
  _ <- symbol "="
  v <- parseConstant
  _ <- some $ symbol ";"
  return (T.unpack k, v)

partitionTopLevelStatements :: [TopLevelStatement] -> ([ImportStatement], [PackageSpecification], [OptionDefinition])
partitionTopLevelStatements = foldr acc initVal
  where
    initVal = ([], [], [])
    acc (ImportStmt e) ~(importSts, packages, opts) = (e : importSts, packages, opts)
    acc (PackageSpec e) ~(importSts, packages, opts) = (importSts, e : packages, opts)
    acc (OptionDef e) ~(importSts, packages, opts) = (importSts, packages, e : opts)
    acc _ ~(importSts, packages, opts) = (importSts, packages, opts)

protoParser :: Parser ProtoFile
protoParser = do
  sy <- parseSyntax
  tls <-
    many $
      choice
        [ PackageSpec <$> try parsePackageSpecification,
          ImportStmt <$> try parseImportStatement,
          OptionDef <$> try parseOptionDefinition
        ]
  _ <- eof
  let (im, pa, op) = partitionTopLevelStatements tls
  return (ProtoFile sy pa im op)

parseProto :: String -> String -> Either String ProtoFile
parseProto f i = case parse protoParser f (T.pack i) of
  Left b -> Left $ errorBundlePretty b
  -- TODO check if there are multiple package definitions present
  Right p -> Right p
