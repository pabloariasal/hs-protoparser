module HSProtoParser.Ast
  ( ProtoFile (..),
    TopLevelDefinition (..),
    SyntaxStatement,
    PackageSpecification,
    OptionDefinition,
    EnumDefinition (..),
    EnumField (..),
    Constant (..),
    ImportStatement (..),
    AccessQualifier (..),
    MessageDefinition (..),
    ServiceDefinition (..),
  )
where

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

data MessageDefinition = MessageDefinition deriving (Eq, Show)

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
  = MessageDef MessageDefinition
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
