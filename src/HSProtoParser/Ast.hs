module HSProtoParser.Ast
  ( ProtoFile (..),
    SyntaxDefinition,
    PackageDefinition,
    ImportStatement (..),
    AccessQualifier (..),
    TopLevelStatement (..),
  )
where

type PackageDefinition = String

type SyntaxDefinition = String

data AccessQualifier = Public | Weak deriving (Eq, Show)

data ImportStatement = ImportStatement (Maybe AccessQualifier) String deriving (Eq, Show)

data Message = Message deriving (Eq, Show)

data Enumeration = Enumeration deriving (Eq, Show)

data Service = Service deriving (Eq, Show)

data TopLevelStatement
  = PackageDef PackageDefinition
  | ImportStmt ImportStatement
  | MessageDef Message
  | EnumDef Enumeration
  | ServiceDef Service
  deriving (Eq, Show)

data ProtoFile = ProtoFile
  { syntax :: SyntaxDefinition,
    package :: [PackageDefinition],
    imports :: [ImportStatement]
  }
  deriving (Eq, Show)
