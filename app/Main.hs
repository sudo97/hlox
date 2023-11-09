{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AST (RuntimeError (RuntimeError), evaluate, printLiteral)
import Control.Exception (catch)
import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import FunctionalParser (parseExpr)
import GHC.IO.Handle (hFlush)
import IdiomaticScanner (ScanError (..), Token (..), scanner)
import Parser (LoxParseError (LoxParseError))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdout)

report :: LoxError -> IO ()
report (ScanErr (ScanError {line, message, place})) = hPutStrLn stderr $ "[line " ++ show line ++ "] Error: " ++ show place ++ ": " ++ message
report (ParseErr (LoxParseError msg tok)) = case tok of
  Just (Token {line}) -> hPutStrLn stderr $ "[line " ++ show line ++ "] Error: " ++ msg
  _ -> hPutStrLn stderr $ "Error: " ++ msg
report (RuntimeErr (RuntimeError message tok)) = hPutStrLn stderr $ "[line " ++ show (tok.line) ++ "] Error: " ++ message

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
      -- print line
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

data LoxError = ParseErr LoxParseError | ScanErr ScanError | RuntimeErr RuntimeError

run :: T.Text -> IO [LoxError]
run source = case scanner' >=> parseExpr' >=> evaluate' $ source of
  Right value -> TIO.putStrLn (printLiteral value) $> []
  Left e -> pure e
  where
    scanner' = mapLeft (ScanErr <$>) . scanner
    parseExpr' = mapLeft (ParseErr <$>) . parseExpr
    evaluate' = mapLeft (RuntimeErr <$>) . evaluate

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right c) = Right c

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [script] -> runFile script
    _ -> do
      putStrLn "Usage: hlox [script]"
      exitFailure