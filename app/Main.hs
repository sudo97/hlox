{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AST (printExpr)
import Control.Exception (catch)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (hFlush)
import IdiomaticScanner (ScanErr (..), Token (..), scanner)
import Parser (ParseError (ParseError), parse)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdout)

report :: Either ScanErr ParseError -> IO ()
report (Left (ScanErr {line, message, place})) = hPutStrLn stderr $ "[line " ++ show line ++ "] Error: " ++ show place ++ ": " ++ message
report (Right (ParseError msg tok)) = case tok of
  Just (Token {line}) -> hPutStrLn stderr $ "[line " ++ show line ++ "] Error: " ++ msg
  _ -> hPutStrLn stderr $ "Error: " ++ msg

runPrompt :: IO ()
runPrompt = catch loop handler
  where
    handler :: IOError -> IO ()
    handler _ = putStrLn "\nBye!"
    loop :: IO ()
    loop = do
      putStr "> "
      hFlush stdout
      line <- getLine
      print line
      if null line
        then pure ()
        else do
          errors <- run $ T.pack line
          traverse_ report errors
          loop

runFile :: String -> IO ()
runFile path = do
  file <- TIO.readFile path
  errors <- run file
  case errors of
    [] -> pure ()
    _ -> do
      traverse_ report errors
      exitFailure

run :: T.Text -> IO [Either ScanErr ParseError]
run source = do
  case scanner source of
    Right tokens -> do
      case parse tokens of
        Right expr -> do
          putStrLn "Success!"
          print expr
          TIO.putStrLn (printExpr expr) $> []
        Left errors -> pure [Right errors]
    Left errors -> pure $ Left <$> errors

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [script] -> runFile script
    _ -> do
      putStrLn "Usage: hlox [script]"
      exitFailure