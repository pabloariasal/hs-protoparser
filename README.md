# hs-protoparser

Haskell library for parsing Protocol Buffer files.

# Installation

The package is not in hackage yet, but you can you add it as an `extra-dep` on stack:

```sh
extra-deps:
  - git: https://github.com/pabloariasal/hs-protoparser.git
    commit: ...
```

# Usage

```haskell
import HSProtoParser.Parser (parseProto)
import HSProtoParser.Ast
import System.Exit

main = do
    f <- readFile "example.proto"
    case parseProto "example.proto" f of
        Left e -> putStr e >> exitFailure
        Right t -> putStr show (getMessages t) >> exitSuccess

getMessages :: ProtoFile -> [String]
getMessages = foldr f []
  where
    f (MsgDef (MessageDefinition n _)) acc = n:acc
    f _ acc = acc
```

# Contributing
