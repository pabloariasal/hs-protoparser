# hs-protoparser

Parser for Protocol Buffer files written in Haskell.

```haskell
> cat file.proto
syntax = "proto3";
message M
{
    string name = 1;
}

> stack exec hs-protoparser-exe file.proto
ProtoFile
    { syntax = "proto3"
    , packages = []
    , imports = []
    , options = []
    , messages =
        [ MessageDefinition
            { name = "M"
            , oneOfFields = []
            , mapFields = []
            , normalFields =
                [ NormalField
                    { field = FieldDefinition
                        { name = "name"
                        , fieldType = TString
                        , fieldNumber = 1
                        , options = []
                        }
                    , repeated = False
                    }
                ]
            , messageDefs = []
            , enumDefs = []
            , options = []
            , reservedFields = []
            }
        ]
    , enums = []
    , services = []
    }

```

# Installation

If you are using stack you can add the package as an `extra-dep`:

```sh
extra-deps:
  - git: https://github.com/pabloariasal/hs-protoparser.git
    commit: ...
```

# Usage

Call `parseProto` from the `Parser` module with the input:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}

import HSProtoParser.Parser (parseProto)
import HSProtoParser.Ast
import System.Exit

main = do
    f <- readFile "example.proto"
    case parseProto "example.proto" f of
        Left e -> print e >> exitFailure
        Right t -> print (map n (messages t)) >> exitSuccess
    where
        n :: MessageDefinition -> String
        n = name
```
This will print the names of all messages defined in the input proto:

```bash
> cat example.proto
syntax = "proto3";
message A {}
message B {}
message C {}

> stack run
["A", "B", "C"]
```
