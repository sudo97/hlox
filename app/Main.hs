{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch)
import Control.Monad (unless, (>=>))
import Control.Monad.Except (MonadIO (liftIO))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Eval
import FunctionalParser (parseProgram)
import GHC.IO.Handle (hFlush)
import IdiomaticScanner (ScanError (..), Token (..), scanner)
import Parser (LoxParseError (LoxParseError), RuntimeError (RuntimeError), Stmt)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdout)

data LoxError = ParseErr LoxParseError | ScanErr ScanError | RuntimeErr RuntimeError

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [script] -> runFile script
    _ -> do
      putStrLn "Usage: hlox [script]"
      exitFailure

runPrompt :: IO ()
runPrompt = go `catch` handler
  where
    handler :: IOError -> IO ()
    handler _ = putStrLn "\nBye!"
    go :: IO ()
    go = do
      result <- runEval loop
      case result of
        Left e -> report (RuntimeErr e) >> go
        Right _ -> pure ()
    loop :: Eval ()
    loop = do
      liftIO $ putStr "hlox> "
      liftIO $ hFlush stdout
      line <- liftIO TIO.getLine
      unless (T.null line) $ do
        case getAst line of
          Left err -> liftIO $ traverse_ report err
          Right ast -> traverse_ runStmt ast
        loop

runFile :: String -> IO ()
runFile path = do
  file <- TIO.readFile path
  case getAst file of
    Right ast -> do
      result <- runEval $ traverse_ runStmt ast
      case result of
        Left e -> report (RuntimeErr e)
        Right _ -> pure ()
    Left err -> traverse_ report err

getAst :: T.Text -> Either [LoxError] [Stmt]
getAst = mapLeft (ScanErr <$>) . scanner >=> mapLeft (ParseErr <$>) . parseProgram

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right c) = Right c

report :: LoxError -> IO ()
report (ScanErr (ScanError {line, message, place})) = hPutStrLn stderr $ "[line " ++ show line ++ "] Error: " ++ show place ++ ": " ++ message
report (ParseErr (LoxParseError msg tok)) = case tok of
  Just (Token {line}) -> hPutStrLn stderr $ "[line " ++ show line ++ "] Error: " ++ msg
  _ -> hPutStrLn stderr $ "Error: " ++ msg
report (RuntimeErr (RuntimeError message tok)) = hPutStrLn stderr $ "[line " ++ show (tok.line) ++ "] Error: " ++ message
