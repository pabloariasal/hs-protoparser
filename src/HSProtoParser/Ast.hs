module HSProtoParser.Ast
  ( ProtoFile (..),
  )
where

type Package = [String]

data ProtoFile = ProtoFile
  { syntax :: String,
    package :: Package
  }
  deriving (Eq, Show)
