module HSProtoParser.Ast
  ( ProtoFile (..),
  )
where

data ProtoFile = ProtoFile
  { syntax :: String,
    package :: Maybe String
  }
  deriving (Eq, Show)
