{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HSProtoParser.Parser
  ( protoParser,
    parseProto,
  )
where

import Data.Maybe (isJust)
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
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

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
parseOptionName = T.append <$> (try simple <|> surrounded) <*> (T.concat <$> many suffix)
  where
    simple = ident
    surrounded = T.concat <$> (createTripleList <$> symbol "(" <*> parseFullIdent <*> symbol ")")
    suffix = T.append <$> (T.singleton <$> char '.') <*> ident
    createTripleList a b c = [a, b, c]

parseOptionDefinition :: Parser OptionDefinition
parseOptionDefinition = do
  _ <- symbol "option"
  k <- parseOptionName
  _ <- symbol "="
  v <- parseConstant
  _ <- some $ symbol ";"
  return (T.unpack k, v)

parseFieldOption :: Parser OptionDefinition
parseFieldOption = do
  n <- T.unpack <$> parseOptionName
  _ <- symbol "="
  k <- parseConstant
  return (n, k)

parseFieldOptions :: Parser [OptionDefinition]
parseFieldOptions = between (symbol "[") (symbol "]") (parseFieldOption `sepBy1` symbol ",") <|> [] <$ lookAhead (symbol ";")

parseEnumField :: Parser EnumField
parseEnumField = do
  n <- T.unpack <$> ident
  _ <- symbol "="
  v <- signedInteger
  o <- parseFieldOptions
  _ <- some $ symbol ";"
  return (EnumField n v o)

parseEnumElements :: Parser [EnumElement]
parseEnumElements = ([] <$ some (symbol ";")) <|> many (EnOpt <$> parseOptionDefinition <|> EnField <$> parseEnumField)

parseEnumDefinition :: Parser EnumDefinition
parseEnumDefinition = do
  _ <- symbol "enum"
  n <- T.unpack <$> ident
  e <- between (symbol "{") (symbol "}") parseEnumElements
  _ <- many $ symbol ";"
  return (EnumDefinition n e)

parseFieldNumberSpec :: Parser FieldNumberSpec
parseFieldNumberSpec = try singleNum <|> range
  where
    singleNum = Single <$> integer <* notFollowedBy (symbol "to")
    range = Range <$> integer <*> (symbol "to" *> integer)

parseReservedFieldNumbersStatement :: Parser [FieldNumberSpec]
parseReservedFieldNumbersStatement = symbol "reserved" *> parseFieldNumberSpec `sepBy1` symbol "," <* some (symbol ";")

parseKeyType :: Parser KeyType
parseKeyType =
  choice
    [ try $ KTInt32 <$ symbol "int32",
      try $ KTInt64 <$ symbol "int64",
      try $ KTUInt32 <$ symbol "uint32",
      try $ KTUInt64 <$ symbol "uint64",
      try $ KTSInt32 <$ symbol "sint32",
      try $ KTSInt64 <$ symbol "sint64",
      try $ KTFixed32 <$ symbol "fixed32",
      try $ KTFixed64 <$ symbol "fixed64",
      try $ KTSfixed32 <$ symbol "sfixed32",
      try $ KTSfixed64 <$ symbol "sfixed64",
      try $ KTBool <$ symbol "bool",
      try $ KTString <$ symbol "string"
    ]

parseMapFieldTypes :: Parser (KeyType, FieldType)
parseMapFieldTypes = between (symbol "<") (symbol ">") ((,) <$> parseKeyType <*> (symbol "," *> parseFieldType))

parseFieldInfo :: Parser (String, Int, [OptionDefinition])
parseFieldInfo = (,,) <$> (T.unpack <$> ident) <*> (symbol "=" *> integer) <*> parseFieldOptions

parseMapField :: Parser MapField
parseMapField = do
  _ <- symbol "map"
  (kt, vt) <- parseMapFieldTypes
  (fieldName, num, opts) <- parseFieldInfo
  _ <- some $ symbol ";"
  return $ MapField fieldName kt vt num opts

parseOneOfElements :: Parser [OneOfFieldElement]
parseOneOfElements = [] <$ some (symbol ";") <|> many (OFFieldDef <$> try parseFieldDefinition <|> OFOptDef <$> parseOptionDefinition)

parseOneOfField :: Parser OneOfField
parseOneOfField = do
  _ <- symbol "oneof"
  n <- T.unpack <$> ident
  e <- between (symbol "{") (symbol "}") parseOneOfElements
  _ <- many $ symbol ";"
  return $ OneOfField n e

parseFieldType :: Parser FieldType
parseFieldType =
  choice
    [ try $ FTDouble <$ symbol "double",
      try $ FTFloat <$ symbol "float",
      try $ FTInt32 <$ symbol "int32",
      try $ FTInt64 <$ symbol "int64",
      try $ FTUInt32 <$ symbol "uint32",
      try $ FTUInt64 <$ symbol "uint64",
      try $ FTSInt32 <$ symbol "sint32",
      try $ FTSInt64 <$ symbol "sint64",
      try $ FTFixed32 <$ symbol "fixed32",
      try $ FTFixed64 <$ symbol "fixed64",
      try $ FTSfixed32 <$ symbol "sfixed32",
      try $ FTSfixed64 <$ symbol "sfixed64",
      try $ FTBool <$ symbol "bool",
      try $ FTString <$ symbol "string",
      try $ FTBytes <$ symbol "bytes",
      try $ FTMessageType . T.unpack <$> parseFullIdent
    ]

parseFieldDefinition :: Parser FieldDefinition
parseFieldDefinition = do
  t <- parseFieldType
  (fieldName, num, opts) <- parseFieldInfo
  _ <- some $ symbol ";"
  return $ FieldDefinition fieldName t num opts

parseNormalField :: Parser NormalField
parseNormalField = do
  r <- optional . try $ symbol "repeated"
  f <- parseFieldDefinition
  return $ NormalField f (isJust r)

parseMessageElements :: Parser [MessageElement]
parseMessageElements =
  [] <$ some (symbol ";")
    <|> many
      ( choice
          [ NorF <$> try parseNormalField,
            MapF <$> try parseMapField,
            OneF <$> try parseOneOfField,
            Msg <$> try parseMessageDefinition,
            Enum <$> try parseEnumDefinition,
            Opt <$> try parseOptionDefinition,
            RsvFieldNums <$> try parseReservedFieldNumbersStatement
          ]
      )

parseMessageDefinition :: Parser MessageDefinition
parseMessageDefinition = do
  _ <- symbol "message"
  n <- T.unpack <$> ident
  e <- between (symbol "{") (symbol "}") parseMessageElements
  _ <- many $ symbol ";"
  return $ MessageDefinition n e

data TopLevelStatement
  = PackageSpec PackageSpecification
  | ImportStmt ImportStatement
  | OptionDef OptionDefinition
  | TopLevelDef TopLevelDefinition
  deriving (Eq, Show)

partitionTopLevelStatements :: [TopLevelStatement] -> ([ImportStatement], [PackageSpecification], [OptionDefinition], [TopLevelDefinition])
partitionTopLevelStatements = foldr acc initVal
  where
    initVal = ([], [], [], [])
    acc (ImportStmt e) ~(im, pa, op, en) = (e : im, pa, op, en)
    acc (PackageSpec e) ~(im, pa, op, en) = (im, e : pa, op, en)
    acc (OptionDef e) ~(im, pa, op, en) = (im, pa, e : op, en)
    acc (TopLevelDef e) ~(im, pa, op, en) = (im, pa, op, e : en)

protoParser :: Parser ProtoFile
protoParser = do
  sy <- parseSyntax
  tls <-
    many $
      choice
        [ PackageSpec <$> try parsePackageSpecification,
          ImportStmt <$> try parseImportStatement,
          OptionDef <$> try parseOptionDefinition,
          TopLevelDef . EnumDef <$> try parseEnumDefinition,
          TopLevelDef . MsgDef <$> try parseMessageDefinition
        ]
  _ <- eof
  let (im, pa, op, en) = partitionTopLevelStatements tls
  return (ProtoFile sy pa im op en)

parseProto :: String -> String -> Either String ProtoFile
parseProto f i = case parse protoParser f (T.pack i) of
  Left b -> Left $ errorBundlePretty b
  -- TODO check if there are multiple package definitions present
  Right p -> Right p
