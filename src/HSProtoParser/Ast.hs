{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module HSProtoParser.Ast where

type PackageSpecification = String

type SyntaxStatement = String

type FileName = String

data Constant
  = Identifier String
  | FloatLit Float
  | IntLit Int
  | StringLit String
  | BoolLit Bool
  deriving (Eq, Show)

data AccessQualifier = Public | Weak deriving (Eq, Show)

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

data FieldDefinition = FieldDefinition
  { name :: String,
    fieldType :: FieldType,
    fieldNumber :: Int,
    options :: [OptionDefinition]
  }
  deriving (Eq, Show)

data OneOfFieldElement = OFFieldDef FieldDefinition | OFOptDef OptionDefinition deriving (Eq, Show)
data OneOfField = OneOfField
  { name :: String,
    elements :: [OneOfFieldElement]
  }
  deriving (Eq, Show)

data KeyType
  = KTInt32
  | KTInt64
  | KTUInt32
  | KTUInt64
  | KTSInt32
  | KTSInt64
  | KTFixed32
  | KTFixed64
  | KTSfixed32
  | KTSfixed64
  | KTBool
  | KTString
  deriving (Eq, Show)

data MapField = MapField
  { name :: String,
    keyType :: KeyType,
    valueType :: FieldType,
    fieldNumber :: Int,
    options :: [OptionDefinition]
  }
  deriving (Eq, Show)

data NormalField = NormalField
  { field :: FieldDefinition,
    repeated :: Bool
  }
  deriving (Eq, Show)

data FieldNumberSpec = Single Int | Range Int Int deriving (Eq, Show)

data MessageElement
  = OneF OneOfField
  | MapF MapField
  | NorF NormalField
  | Msg MessageDefinition
  | Enum EnumDefinition
  | Opt OptionDefinition
  | RsvFieldNums [FieldNumberSpec]
  | RsvFieldNames [String]
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

data EnumElement = EnOpt OptionDefinition | EnField EnumField deriving (Eq, Show)

data EnumDefinition = EnumDefinition
  { name :: String,
    elements :: [EnumElement]
  }
  deriving (Eq, Show)

data ServiceDefinition = ServiceDefinition deriving (Eq, Show)

type OptionDefinition = (String, Constant)

data ProtoFileElement
  = SyntaxStmt SyntaxStatement
  | PackageSpec PackageSpecification
  | ImportStmt (Maybe AccessQualifier) FileName
  | OptionDef OptionDefinition
  | MsgDef MessageDefinition
  | EnumDef EnumDefinition
  | ServiceDef ServiceDefinition
  deriving (Show, Eq)

type ProtoFile = [ProtoFileElement]
