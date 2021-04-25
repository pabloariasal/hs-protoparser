module HSProtoParser.Ast
  ( ProtoFile (..),
    SyntaxStatement,
    PackageSpecification,
    OptionDefinition,
    Constant (..),
    ImportStatement (..),
    AccessQualifier (..),
    TopLevelStatement (..),
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

data Enumeration = Enumeration deriving (Eq, Show)

data Service = Service deriving (Eq, Show)

type OptionDefinition = (String, Constant)

data TopLevelStatement
  = PackageSpec PackageSpecification
  | ImportStmt ImportStatement
  | MessageDef Message
  | EnumDef Enumeration
  | ServiceDef Service
  | OptionDef OptionDefinition
  deriving (Eq, Show)

data ProtoFile = ProtoFile
  { syntaxStmt :: SyntaxStatement,
    packageSpec :: [PackageSpecification],
    importStmts :: [ImportStatement],
    optionDefs :: [OptionDefinition]
  }
  deriving (Eq, Show)
