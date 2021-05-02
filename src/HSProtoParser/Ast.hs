{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module HSProtoParser.Ast where

type PackageSpecification = String

type SyntaxStatement = String

data Constant
  = Identifier String
  | FloatLit Float
  | IntLit Int
  | StringLit String
  | BoolLit Bool
  deriving (Eq, Show)

data AccessQualifier = Public | Weak deriving (Eq, Show)

data ImportStatement = ImportStatement (Maybe AccessQualifier) String deriving (Eq, Show)

data OneOfField = OneOfField deriving (Eq, Show)

data MapField = MapField deriving (Eq, Show)

data FieldType
  = FTDouble
  | FTFloat
  | FTInt32
  | FTInt64
  | FTUInt32
  | FTUInt64
  | FTSInt32
  | FTSInt64
  | FTFixed32
  | FTFixed64
  | FTSfixed32
  | FTSfixed64
  | FTBool
  | FTString
  | FTBytes
  | FTMessageType String
  deriving (Eq, Show)

data NormalField = NormalField
  { name :: String,
    fieldType :: FieldType,
    fieldNumber :: Int,
    options :: [OptionDefinition],
    repeated :: Bool
  }
  deriving (Eq, Show)

data ReservedStatement = ReservedStatement deriving (Eq, Show)

data MessageElement
  = OneF OneOfField
  | MapF MapField
  | NorF NormalField
  | Msg MessageDefinition
  | Enum EnumDefinition
  | Opt OptionDefinition
  | Rsv ReservedStatement
  deriving (Eq, Show)

data MessageDefinition = MessageDefinition
  { name :: String,
    elements :: [MessageElement]
  }
  deriving (Eq, Show)

data EnumField = EnumField
  { identifier :: String,
    value :: Int,
    enumValOpts :: [OptionDefinition]
  }
  deriving (Eq, Show)

data EnumDefinition = EnumDefinition
  { name :: String,
    options :: [OptionDefinition],
    fields :: [EnumField]
  }
  deriving (Eq, Show)

data ServiceDefinition = ServiceDefinition deriving (Eq, Show)

type OptionDefinition = (String, Constant)

data TopLevelDefinition
  = MsgDef MessageDefinition
  | EnumDef EnumDefinition
  | ServiceDef ServiceDefinition
  deriving (Show, Eq)

data ProtoFile = ProtoFile
  { syntaxStmt :: SyntaxStatement,
    packageSpec :: [PackageSpecification],
    importStmts :: [ImportStatement],
    optionDefs :: [OptionDefinition],
    topLevelDefs :: [TopLevelDefinition]
  }
  deriving (Eq, Show)
