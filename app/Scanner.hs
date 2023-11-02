{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scanner
  ( scanTokens,
    ScanErr (..),
    TokenType (..),
    Token (..),
  )
where

import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Foldable (Foldable (..))
import Data.Functor (($>))
import Data.Sequence (Seq, (|>))
import qualified Data.Text as T

data ScanErr = ScanErr {line :: Int, message :: String, place :: String}
  deriving (Show)

data TokenType
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier T.Text
  | String T.Text
  | Number Double
  | And
  | Class
  | Else
  | FalseTok
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | TrueTok
  | Var
  | While
  | EOF
  deriving (Show, Eq)

data Token = Token {tokenType :: TokenType, line :: Int}
  deriving (Show)

data ScannerState = ScannerState
  { start :: Int,
    current :: Int,
    line :: Int,
    tokens :: Seq Token,
    source :: T.Text,
    errors :: Maybe (Seq ScanErr)
  }
  deriving (Show)

type Scanner a = State ScannerState a

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb m = mb >>= \b -> when b m

whileM :: Monad m => m Bool -> m () -> m ()
whileM mb m = mb >>= \b -> when b $ m >> whileM mb m

isAtEnd :: Scanner Bool
isAtEnd = do
  gets $ \ScannerState {current, source} -> current >= T.length source

err :: String -> Scanner ()
err s = do
  modify $ \st ->
    st
      { errors = case st.errors of
          Just e -> Just $ e |> ScanErr (st.line) s ""
          Nothing -> Just $ mempty |> ScanErr (st.line) s ""
      }

scanTokens :: T.Text -> Either [ScanErr] [Token]
scanTokens source =
  let (_, b) = runState scanTokens' (ScannerState 0 0 1 mempty source Nothing)
   in case b.errors of
        Just e -> Left (toList e)
        Nothing -> Right (toList b.tokens)
  where
    scanTokens' :: Scanner ()
    scanTokens' = whileM (not <$> isAtEnd) $ do
      modify $ \st -> st {start = current st}
      scanToken

advance :: Scanner Char
advance = do
  ScannerState {..} <- get
  let ch = T.index source current
  put $ ScannerState {current = current + 1, ..}
  pure ch

-- pure $ T.index source (current - 1)

addToken :: TokenType -> Scanner ()
addToken tokenType = do
  st <- get
  let token = Token tokenType (st.line)
  put $ st {tokens = st.tokens |> token}

match :: Char -> Scanner Bool
match c = do
  st <- get
  end <- isAtEnd
  if end
    then pure False
    else do
      let c' = T.index st.source st.current
      if c /= c'
        then pure False
        else do
          put $ st {current = current st + 1}
          pure True

peek :: Scanner Char
peek = do
  end <- isAtEnd
  if end
    then pure '\0'
    else gets $ \ScannerState {source, current} -> T.index source current

string :: Scanner ()
string = do
  let cond = (&&) <$> (not <$> isAtEnd) <*> ((/= '"') <$> peek)
  whileM cond $ do
    whenM ((== '\n') <$> peek) . modify $ \ScannerState {..} -> ScannerState {line = line + 1, ..}
    advance $> ()

  end <- isAtEnd
  if end
    then err "Unterminated string."
    else do
      advance $> () -- The closing ".
      ScannerState {..} <- get
      let str = T.take (current - start + 1) $ T.drop (start - 1) source
      addToken $ String str

peekNext :: Scanner Char
peekNext =
  gets $ \ScannerState {..} ->
    if current + 1 >= T.length source
      then '\0'
      else T.index source (current + 1)

number :: Scanner ()
number = do
  whileM (isDigit <$> peek) $ advance $> ()

  -- Look for a fractional part.
  nextIsDigit <- isDigit <$> peekNext
  peekIsDot <- (== '.') <$> peek
  when (nextIsDigit && peekIsDot) $ do
    -- Consume the "."
    advance $> ()

    whileM (isDigit <$> peek) $ advance $> ()

  ScannerState {..} <- get
  let num = read (T.unpack $ T.take (current - start) $ T.drop start source) -- Shouldn't fail, because we only advance if isDigit
  addToken $ Number num

identifier :: Scanner ()
identifier = do
  whileM (isAlphaNum <$> peek) $ advance $> ()
  ScannerState {..} <- get
  let text = T.take (current - start) $ T.drop start source
  case text of
    "and" -> addToken And
    "class" -> addToken Class
    "else" -> addToken Else
    "false" -> addToken FalseTok
    "for" -> addToken For
    "fun" -> addToken Fun
    "if" -> addToken If
    "nil" -> addToken Nil
    "or" -> addToken Or
    "print" -> addToken Print
    "return" -> addToken Return
    "super" -> addToken Super
    "this" -> addToken This
    "true" -> addToken TrueTok
    "var" -> addToken Var
    "while" -> addToken While
    _ -> addToken (Identifier text)

scanToken :: Scanner ()
scanToken = do
  c <- advance
  case c of
    '(' -> addToken LeftParen
    ')' -> addToken RightParen
    '{' -> addToken LeftBrace
    '}' -> addToken RightBrace
    ',' -> addToken Comma
    '.' -> addToken Dot
    '-' -> addToken Minus
    '+' -> addToken Plus
    ';' -> addToken Semicolon
    '*' -> addToken Star
    '!' -> do
      matches <- match '='
      if matches then addToken BangEqual else addToken Bang
    '=' -> do
      matches <- match '='
      if matches then addToken EqualEqual else addToken Equal
    '<' -> do
      matches <- match '='
      if matches then addToken LessEqual else addToken Less
    '>' -> do
      matches <- match '='
      if matches then addToken GreaterEqual else addToken Greater
    '/' -> do
      matches <- match '/'
      if matches
        then do
          whileM ((&&) <$> (not <$> isAtEnd) <*> (('\n' /=) <$> peek)) $ advance $> ()
        else addToken Slash
    ' ' -> pure ()
    '\r' -> pure ()
    '\t' -> pure ()
    '\n' -> modify $ \ScannerState {..} -> ScannerState {line = line + 1, ..}
    '"' -> string
    c' | isDigit c' -> number
    c' | isAlpha c' -> identifier
    _ -> err "Unexpected character."