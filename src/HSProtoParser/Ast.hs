{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module HSProtoParser.Ast where

type PackageSpecification = String

type SyntaxStatement = String

type FileName = String

type Identifier = String

type FieldNumber = Int

type Repeated = Bool

-- TODO add prefix to data constructors
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

-- this is called a "range" in the protobuf specification, but it allows a single field to be a "range"
-- hence I decided to call this "field number specification", to be more generic
data FieldNumberSpec = FSSingle FieldNumber | FSRange FieldNumber FieldNumber deriving (Eq, Show)

data ReservedFieldStatement = RFNumbers [FieldNumberSpec] | RFNames [Identifier] deriving (Eq, Show)

data MessageDefinition = MessageDefinition
  { name :: Identifier,
    oneOfFields :: [OneOfField],
    mapFields :: [MapField],
    normalFields :: [NormalField],
    messageDefs :: [MessageDefinition],
    enumDefs :: [EnumDefinition],
    options :: [OptionDefinition],
    reservedFields :: [ReservedFieldStatement]
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
