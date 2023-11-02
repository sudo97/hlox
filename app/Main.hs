{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (hFlush)
import Scanner (ScanErr (..), scanTokens)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdout)

report :: ScanErr -> IO ()
report (ScanErr {line, message, place}) = hPutStrLn stderr $ "[line " ++ show line ++ "] Error" ++ place ++ ": " ++ message

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

run :: T.Text -> IO [ScanErr]
run source = do
  case scanTokens source of
    Right tokens -> traverse_ print tokens $> mempty
    Left errors -> pure errors

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [script] -> runFile script
    _ -> do
      putStrLn "Usage: hlox [script]"
      exitFailure