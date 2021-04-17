module Main where

import HSProtoParser.Ast
import HSProtoParser.Parser
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["--help"] = printUsage
parseArgs [] = runParser "" getContents
parseArgs [f] = runParser f (readFile f)

printUsage :: IO ()
printUsage = do
  n <- getProgName
  _ <- putStrLn $ "Usage: " ++ n ++ " " ++ "<file.proto>"
  exitSuccess

runParser :: String -> IO String -> IO ()
runParser s i = do
  c <- i
  case parseProto s c of
    Left e -> print e >> exitFailure
    Right t -> print t >> exitSuccess
