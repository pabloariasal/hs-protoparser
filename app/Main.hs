{-# OPTIONS -Wno-missing-export-lists #-}

module Main where

import HSProtoParser.Parser (parseProto)
import System.Environment
import System.Exit
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = getArgs >>= parseArgs

-- main = do
--     f <- readFile "example.proto"
--     case parseProto "example.proto" f of
--         Left e -> putStr e >> exitFailure
--         Right t -> putStr show (getMessages t) >> exitSuccess

-- getMessages :: ProtoFile -> [String]
-- getMessages = foldr f []
--   where
--     f (MsgDef (MessageDefinition n _)) acc = n:acc
--     f _ acc = acc

parseArgs :: [String] -> IO ()
parseArgs ["--help"] = printUsage
parseArgs [] = runParser "" getContents
parseArgs [f] = runParser f (readFile f)
parseArgs _ = printUsage

printUsage :: IO ()
printUsage = do
  n <- getProgName
  _ <- putStrLn $ "Usage: " ++ n ++ " " ++ "<file.proto>"
  exitSuccess

runParser :: String -> IO String -> IO ()
runParser s i = do
  c <- i
  case parseProto s c of
    Left e -> putStr e >> exitFailure
    Right t -> pPrint t >> exitSuccess
