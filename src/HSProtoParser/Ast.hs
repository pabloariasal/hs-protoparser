module HSProtoParser.Ast
  ( ProtoFile (..),
    SyntaxStatement,
    PackageSpecification,
    OptionDefinition,
    EnumDefinition (..),
    EnumField (..),
    Constant (..),
    ImportStatement (..),
    AccessQualifier (..),
    Message (..),
    Service (..)
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

data Message = Message deriving (Eq, Show)

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

data Service = Service deriving (Eq, Show)

type OptionDefinition = (String, Constant)

data ProtoFile = ProtoFile
  { syntaxStmt :: SyntaxStatement,
    packageSpec :: [PackageSpecification],
    importStmts :: [ImportStatement],
    optionDefs :: [OptionDefinition],
    enumDefs :: [EnumDefinition]
  }
  deriving (Eq, Show)
