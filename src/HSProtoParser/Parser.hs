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
-- import Text.Megaparsec.Debug

import HSProtoParser.Ast qualified as Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

consumeSemicolons :: Parser ()
consumeSemicolons = () <$ some (symbol ";")

-- match verbatim strings, consuming trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- match generic lexemes, like ints. Consumes trailing whitespace
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
betweenChar c p = between (char c) (char c) p <* sc

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (stringWithSep '\'' <|> stringWithSep '\"') <* sc
  where
    stringWithSep :: Char -> Parser String
    stringWithSep s = let m = char s in m *> manyTill L.charLiteral m

-- identifiers start with a letter followed by any combination of alphanumeric and '_'
ident :: Parser Text
ident = (T.pack <$> p) <* sc
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

boolLit :: Parser Bool
boolLit = (True <$ symbol "true" <|> False <$ "false") <* sc

-- '.' separated idents
parseFullIdent :: Parser Text
parseFullIdent = T.intercalate (T.singleton '.') <$> (ident `sepBy1` char '.')

parseSyntax :: Parser Ast.SyntaxStatement
parseSyntax = do
  _ <- sc
  _ <- symbol "syntax"
  _ <- symbol "="
  s <- betweenChar '"' (symbol "proto3") <|> betweenChar '\'' (symbol "proto3")
  _ <- consumeSemicolons
  return (T.unpack s)

parsePackageSpecification :: Parser Ast.PackageSpecification
parsePackageSpecification = T.unpack <$> (symbol "package" *> parseFullIdent <* consumeSemicolons)

parseImportStatement :: Parser Ast.ProtoFileElement
parseImportStatement = do
  _ <- symbol "import"
  access <- optional . try $ (Ast.Weak <$ symbol "weak" <|> Ast.Public <$ symbol "public")
  path <- stringLiteral
  _ <- consumeSemicolons
  return $ Ast.ImportStmt access (T.unpack path)

parseConstant :: Parser Ast.Constant
parseConstant =
  choice
    [ Ast.BoolLit <$> try boolLit,
      Ast.StringLit . T.unpack <$> try stringLiteral,
      Ast.Identifier . T.unpack <$> try parseFullIdent,
      Ast.IntLit <$> try signedInteger,
      Ast.FloatLit <$> try signedFloat
    ]

parseOptionName :: Parser Text
parseOptionName = T.append <$> (try simple <|> surrounded) <*> (T.concat <$> many suffix)
  where
    simple = ident
    surrounded = T.concat <$> (createTripleList <$> symbol "(" <*> parseFullIdent <*> symbol ")")
    suffix = T.append <$> (T.singleton <$> char '.') <*> ident
    createTripleList a b c = [a, b, c]

parseOptionDefinition :: Parser Ast.OptionDefinition
parseOptionDefinition = do
  _ <- symbol "option"
  k <- parseOptionName
  _ <- symbol "="
  v <- parseConstant
  _ <- consumeSemicolons
  return (T.unpack k, v)

parseFieldOption :: Parser Ast.OptionDefinition
parseFieldOption = (,) <$> (T.unpack <$> parseOptionName <* symbol "=") <*> parseConstant

parseFieldOptions :: Parser [Ast.OptionDefinition]
parseFieldOptions = between (symbol "[") (symbol "]") (parseFieldOption `sepBy1` symbol ",") <|> [] <$ lookAhead (symbol ";")

parseEnumField :: Parser Ast.EnumField
parseEnumField = do
  n <- T.unpack <$> ident
  _ <- symbol "="
  v <- signedInteger
  o <- parseFieldOptions
  _ <- consumeSemicolons
  return (Ast.EnumField n v o)

parseEnumElements :: Parser [Ast.EnumElement]
parseEnumElements = ([] <$ some (symbol ";")) <|> many (Ast.EnOpt <$> parseOptionDefinition <|> Ast.EnField <$> parseEnumField)

parseEnumDefinition :: Parser Ast.EnumDefinition
parseEnumDefinition = do
  _ <- symbol "enum"
  n <- T.unpack <$> ident
  e <- between (symbol "{") (symbol "}") parseEnumElements
  _ <- many $ symbol ";"
  return (Ast.EnumDefinition n e)

parseFieldNumberSpec :: Parser Ast.FieldNumberSpec
parseFieldNumberSpec = try singleNum <|> range
  where
    singleNum = Ast.FSSingle <$> integer <* notFollowedBy (symbol "to")
    range = Ast.FSRange <$> integer <*> (symbol "to" *> integer)

parseReservedFieldNumbersStatement :: Parser Ast.ReservedFieldStatement
parseReservedFieldNumbersStatement = Ast.RFNumbers <$> (symbol "reserved" *> parseFieldNumberSpec `sepBy1` symbol "," <* some (symbol ";"))

parseFieldNameSpec :: Parser String
parseFieldNameSpec = T.unpack <$> (betweenChar '"' ident <|> betweenChar '\'' ident)

parseReservedFieldNamesStatement :: Parser Ast.ReservedFieldStatement
parseReservedFieldNamesStatement = Ast.RFNames <$> (symbol "reserved" *> parseFieldNameSpec `sepBy1` symbol "," <* some (symbol ";"))

parseKeyType :: Parser Ast.KeyType
parseKeyType =
  choice
    [ try $ Ast.KTInt32 <$ symbol "int32",
      try $ Ast.KTInt64 <$ symbol "int64",
      try $ Ast.KTUInt32 <$ symbol "uint32",
      try $ Ast.KTUInt64 <$ symbol "uint64",
      try $ Ast.KTSInt32 <$ symbol "sint32",
      try $ Ast.KTSInt64 <$ symbol "sint64",
      try $ Ast.KTFixed32 <$ symbol "fixed32",
      try $ Ast.KTFixed64 <$ symbol "fixed64",
      try $ Ast.KTSfixed32 <$ symbol "sfixed32",
      try $ Ast.KTSfixed64 <$ symbol "sfixed64",
      try $ Ast.KTBool <$ symbol "bool",
      try $ Ast.KTString <$ symbol "string"
    ]

parseMapFieldTypes :: Parser (Ast.KeyType, Ast.Type)
parseMapFieldTypes = between (symbol "<") (symbol ">") ((,) <$> parseKeyType <*> (symbol "," *> parseFieldType))

parseFieldInfo :: Parser (String, Int, [Ast.OptionDefinition])
parseFieldInfo = (,,) <$> (T.unpack <$> ident) <*> (symbol "=" *> integer) <*> parseFieldOptions

parseMapField :: Parser Ast.MapField
parseMapField = do
  _ <- symbol "map"
  (kt, vt) <- parseMapFieldTypes
  (fieldName, num, opts) <- parseFieldInfo
  _ <- consumeSemicolons
  return $ Ast.MapField fieldName kt vt num opts

parseOneOfElements :: Parser [Ast.OneOfFieldElement]
parseOneOfElements = [] <$ some (symbol ";") <|> many (Ast.OFFieldDef <$> try parseFieldDefinition <|> Ast.OFOptDef <$> parseOptionDefinition)

parseOneOfField :: Parser Ast.OneOfField
parseOneOfField = do
  _ <- symbol "oneof"
  n <- T.unpack <$> ident
  e <- between (symbol "{") (symbol "}") parseOneOfElements
  _ <- many $ symbol ";"
  return $ Ast.OneOfField n e

parseFieldType :: Parser Ast.Type
parseFieldType =
  choice
    [ try $ Ast.TDouble <$ symbol "double",
      try $ Ast.TFloat <$ symbol "float",
      try $ Ast.TInt32 <$ symbol "int32",
      try $ Ast.TInt64 <$ symbol "int64",
      try $ Ast.TUInt32 <$ symbol "uint32",
      try $ Ast.TUInt64 <$ symbol "uint64",
      try $ Ast.TSInt32 <$ symbol "sint32",
      try $ Ast.TSInt64 <$ symbol "sint64",
      try $ Ast.TFixed32 <$ symbol "fixed32",
      try $ Ast.TFixed64 <$ symbol "fixed64",
      try $ Ast.TSfixed32 <$ symbol "sfixed32",
      try $ Ast.TSfixed64 <$ symbol "sfixed64",
      try $ Ast.TBool <$ symbol "bool",
      try $ Ast.TString <$ symbol "string",
      try $ Ast.TBytes <$ symbol "bytes",
      try $ Ast.TMessageType . T.unpack <$> parseFullIdent
    ]

parseFieldDefinition :: Parser Ast.FieldDefinition
parseFieldDefinition = do
  t <- parseFieldType
  (fieldName, num, opts) <- parseFieldInfo
  _ <- some $ symbol ";"
  return $ Ast.FieldDefinition fieldName t num opts

parseNormalField :: Parser Ast.NormalField
parseNormalField = do
  r <- optional . try $ symbol "repeated"
  f <- parseFieldDefinition
  return $ Ast.NormalField f (isJust r)

-- we need Ast.this intermediate type to able to parse different types of message elements
data MessageElement
  = MEOneF Ast.OneOfField
  | MEMapF Ast.MapField
  | MENorF Ast.NormalField
  | MEMsg Ast.MessageDefinition
  | MEEnum Ast.EnumDefinition
  | MEOpt Ast.OptionDefinition
  | MEReservedFieldStmt Ast.ReservedFieldStatement

parseMessageElements :: Parser [MessageElement]
parseMessageElements =
  [] <$ some (symbol ";")
    <|> many
      ( choice
          [ MENorF <$> try parseNormalField,
            MEMapF <$> try parseMapField,
            MEOneF <$> try parseOneOfField,
            MEMsg <$> try parseMessageDefinition,
            MEEnum <$> try parseEnumDefinition,
            MEOpt <$> try parseOptionDefinition,
            MEReservedFieldStmt <$> try parseReservedFieldNumbersStatement,
            MEReservedFieldStmt <$> try parseReservedFieldNamesStatement
          ]
      )

--TODO change Text for Identifier type aliasj
assembleMessageDefinition :: Text -> [MessageElement] -> Ast.MessageDefinition
assembleMessageDefinition n e =
  Ast.MessageDefinition
    (T.unpack n)
    [f | MEOneF f <- e]
    [f | MEMapF f <- e]
    [f | MENorF f <- e]
    [m | MEMsg m <- e]
    [m | MEEnum m <- e]
    [o | MEOpt o <- e]
    [r | MEReservedFieldStmt r <- e]

parseMessageDefinition :: Parser Ast.MessageDefinition
parseMessageDefinition = do
  _ <- symbol "message"
  n <- ident
  e <- between (symbol "{") (symbol "}") parseMessageElements
  _ <- many $ symbol ";"
  return $ assembleMessageDefinition n e

protoParser :: Parser Ast.ProtoFile
protoParser = do
  sy <- parseSyntax
  e <-
    many $
      choice
        [ Ast.PackageSpec <$> try parsePackageSpecification,
          try parseImportStatement,
          Ast.OptionDef <$> try parseOptionDefinition,
          Ast.EnumDef <$> try parseEnumDefinition,
          Ast.MsgDef <$> try parseMessageDefinition
        ]
  _ <- eof
  return $ Ast.SyntaxStmt sy : e

parseProto :: String -> String -> Either String Ast.ProtoFile
parseProto f i = case parse protoParser f (T.pack i) of
  Left b -> Left $ errorBundlePretty b
  -- TODO check if there are multiple package definitions present
  Right p -> Right p
