module HSProtoParser.Ast
  ( ProtoFile (..),
    SyntaxDefinition,
    PackageSpecification,
    ImportStatement (..),
    AccessQualifier (..),
    TopLevelStatement (..),
  )
where

type PackageSpecification = String

type SyntaxDefinition = String

data AccessQualifier = Public | Weak deriving (Eq, Show)

data ImportStatement = ImportStatement (Maybe AccessQualifier) String deriving (Eq, Show)

data Message = Message deriving (Eq, Show)

data Enumeration = Enumeration deriving (Eq, Show)

data Service = Service deriving (Eq, Show)

data TopLevelStatement
  = PackageDef PackageSpecification
  | ImportStmt ImportStatement
  | MessageDef Message
  | EnumDef Enumeration
  | ServiceDef Service
  deriving (Eq, Show)

data ProtoFile = ProtoFile
  { syntaxDef :: SyntaxDefinition,
    packageSpec :: [PackageSpecification],
    importStmts :: [ImportStatement]
  }
  deriving (Eq, Show)
