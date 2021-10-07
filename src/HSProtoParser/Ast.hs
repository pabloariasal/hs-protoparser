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

data Type
  = TDouble
  | TFloat
  | TInt32
  | TInt64
  | TUInt32
  | TUInt64
  | TSInt32
  | TSInt64
  | TFixed32
  | TFixed64
  | TSfixed32
  | TSfixed64
  | TBool
  | TString
  | TBytes
  | TMessageType String
  deriving (Eq, Show)

data FieldDefinition = FieldDefinition
  { name :: String,
    fieldType :: Type,
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
    valueType :: Type,
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
