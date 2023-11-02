{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module IdiomaticScanner where

import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Scanner (TokenType (..))

-- | This scanner is an alternative to the one presented in the book "Crafting Interpreters".
-- | It is designed to be more idiomatic to Haskell.
data ScanErr = ScanErr {line :: Int, message :: String, place :: Int}
  deriving (Show)

data Token = Token
  { tokenType :: TokenType,
    line :: Int,
    column :: Int
  }
  deriving (Show, Eq)

data ScannerState = ScannerState
  { current :: Int,
    line :: Int
  }
  deriving (Show)

type Scanner a = State ScannerState a

advance :: T.Text -> Scanner ()
advance source = unless (T.null source) $ modify' (\ScannerState {..} -> ScannerState {current = current + 1, ..})

newline :: Scanner ()
newline = modify' (\ScannerState {..} -> ScannerState {line = line + 1, current = 0})

mktok :: TokenType -> Scanner ([Either ScanErr Token] -> [Either ScanErr Token])
mktok t = do
  ScannerState {..} <- get
  pure ((Right $ Token t line current) :)

scanner :: T.Text -> Either [ScanErr] [Token]
scanner source =
  let result = evalState (scanner' source) (ScannerState 0 1)
   in case partitionEithers result of
        ([], r) -> Right r
        (l, _) -> Left l

scanner' :: T.Text -> Scanner [Either ScanErr Token]
scanner' "" = pure []
scanner' source = case (T.head source, T.tail source) of
  ('(', t) -> mktok LeftParen <*> scanner' t <* advance t
  (')', t) -> mktok RightParen <*> scanner' t <* advance t
  ('{', t) -> mktok LeftBrace <*> scanner' t <* advance t
  ('}', t) -> mktok RightBrace <*> scanner' t <* advance t
  (',', t) -> mktok Comma <*> scanner' t <* advance t
  ('.', t) -> mktok Dot <*> scanner' t <* advance t
  ('-', t) -> mktok Minus <*> scanner' t <* advance t
  ('+', t) -> mktok Plus <*> scanner' t <* advance t
  (';', t) -> advance t *> (mktok Semicolon <*> scanner' t)
  ('*', t) -> mktok Star <*> scanner' t <* advance t
  ('!', t) ->
    ( case T.head t of
        '=' -> advance t *> mktok BangEqual <*> scanner' (T.tail t)
        _ -> mktok Bang <*> scanner' t
    )
      <* advance t
  ('=', t) ->
    ( case T.head t of
        '=' -> mktok EqualEqual <*> scanner' (T.tail t) <* advance t
        _ -> mktok Equal <*> scanner' t
    )
      <* advance t
  ('<', t) ->
    ( case T.head t of
        '=' -> advance t *> mktok LessEqual <*> scanner' (T.tail t)
        _ -> mktok Less <*> scanner' t
    )
      <* advance t
  ('>', t) ->
    ( case T.head t of
        '=' -> advance t *> mktok GreaterEqual <*> scanner' (T.tail t)
        _ -> mktok Greater <*> scanner' t
    )
      <* advance t
  ('/', t) ->
    ( case T.head t of
        '/' -> scanner' (T.dropWhile (/= '\n') t) <* newline
        _ -> (mktok Slash <*> scanner' t) <* advance t
    )
  (' ', t) -> advance t *> scanner' t
  ('\r', t) -> advance t *> scanner' t
  ('\t', t) -> advance t *> scanner' t
  ('\n', t) -> newline *> scanner' t
  ('"', t) -> do
    (res, t') <- string t
    (res :) <$> scanner' t'
  (c, _) | isDigit c -> do
    advance source
    (res, t') <- number source
    (res :) <$> scanner' t'
  (c, _) | isAlpha c -> do
    (res, t') <- identifier source
    (res :) <$> scanner' t'
  (c, t) -> do
    ScannerState {..} <- get
    let err = Left $ ScanErr line ("Unexpected character: " ++ [c]) current
    (err :) <$> scanner' t

string :: T.Text -> Scanner (Either ScanErr Token, T.Text)
string source = do
  let (str, t) = T.breakOn "\"" source
  ScannerState {..} <- get
  let newLineCount = T.count "\n" str
  let newCurrent = current + T.length (last $ T.lines str)
  put $ ScannerState {line = line + newLineCount, current = newCurrent, ..}
  if T.null t
    then pure (Left $ ScanErr line "Unterminated string" current, t)
    else do
      advance t
      pure (Right $ Token (String str) line current, T.tail t)

number :: T.Text -> Scanner (Either ScanErr Token, T.Text)
number source = do
  let (num, t) = T.span isDigit source
  ScannerState {..} <- get
  let newCurrent = current + T.length num
  put $ ScannerState {current = newCurrent, ..}
  if T.null t || T.head t /= '.'
    then pure (Right $ Token (Number . read $ T.unpack num) line current, t)
    else do
      let (num', t') = T.span isDigit $ T.tail t
      let newCurrent' = newCurrent + T.length num'
      put $ ScannerState {current = newCurrent', ..}
      pure (Right $ Token (Number $ read $ T.unpack $ num <> "." <> num') line current, t')

identifier :: T.Text -> Scanner (Either ScanErr Token, T.Text)
identifier source = do
  let (ident, t) = T.span isAlphaNum source
  ScannerState {..} <- get
  let newCurrent = current + T.length ident
  put $ ScannerState {current = newCurrent, ..}
  pure $ case ident of
    "and" -> (Right $ Token And line current, t)
    "class" -> (Right $ Token Class line current, t)
    "else" -> (Right $ Token Else line current, t)
    "false" -> (Right $ Token FalseTok line current, t)
    "for" -> (Right $ Token For line current, t)
    "fun" -> (Right $ Token Fun line current, t)
    "if" -> (Right $ Token If line current, t)
    "nil" -> (Right $ Token Nil line current, t)
    "or" -> (Right $ Token Or line current, t)
    "print" -> (Right $ Token Print line current, t)
    "return" -> (Right $ Token Return line current, t)
    "super" -> (Right $ Token Super line current, t)
    "this" -> (Right $ Token This line current, t)
    "true" -> (Right $ Token TrueTok line current, t)
    "var" -> (Right $ Token Var line current, t)
    "while" -> (Right $ Token While line current, t)
    _ -> (Right $ Token (Identifier ident) line current, t)

simpleIncompleteTest :: IO ()
simpleIncompleteTest = do
  let input = "var x = 10; print x;"
  let expectedTokens = [Token Var 1 0, Token (Identifier "x") 1 4, Token Equal 1 6, Token (Number 10) 1 8, Token Semicolon 1 11, Token Print 1 12, Token (Identifier "x") 1 18, Token Semicolon 1 19]
  case scanner input of
    (Right tokens) -> do
      traverse_
        ( \(e, t) -> do
            putStrLn $ "Expected: " <> show e
            putStrLn $ "Actual:   " <> show t
            putStrLn "========================="
        )
        (zip expectedTokens tokens)
      if tokens == expectedTokens
        then putStrLn "Test passed"
        else putStrLn "Test failed"
    (Left e) -> do
      traverse_ print e
      putStrLn "Test failed"
