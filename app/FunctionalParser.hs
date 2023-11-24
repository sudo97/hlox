{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalParser where

import Data.List (partition)
import qualified Data.Text as T
import IdiomaticScanner (Token (..), scanner)
import Parser (Expr (..), LiteralValue (..), Stmt (..))
import qualified Parser as P
import Scanner (TokenType (..))
import Text.Parsec
import Text.Parsec.Pos (newPos)
import Text.Parsec.Prim ()

type Parser a = Parsec [Token] () a

-- | The grammar for the parser is now:
-- >
-- > program   -> statement* EOF ;
-- >
-- > statement -> exprStmt
-- >              | printStmt ;
-- >
-- > exprStmt  -> expression ";" ;
-- > printStmt -> "print" expression ";" ;
program :: Parser [Stmt]
program = manyTill (declaration <|> invalidStmt) eof'
  where
    declaration = varDecl <|> statement
    varDecl = do
      _ <- varTok
      name <- identifierTok
      expr <- optionMaybe (equal *> expression)
      _ <- semicolon
      pure $ case expr of
        Just e -> VarDecl name e
        Nothing -> VarDecl name (Literal NilValue)

    statement = expression' <|> printStmt <|> block
    expression' = Expression <$> (try expression <* semicolon)
    printStmt = Parser.Print <$> (printTok *> expression <* semicolon)
    invalidStmt = do
      -- This is ugly. We basically re-parse the statement, knowing
      -- that it will fail, but this time internally to be able to
      -- catch the error and return it as an Invalid Stmt.
      stm <- (\xs x -> xs ++ [x]) <$> many notSemicolon <*> semicolon
      case parse statement "" stm of
        Left err -> pure $ InvalidStmt (P.LoxParseError (show err) Nothing)
        Right _ -> error "This is impossible"
    block = Block <$> (lbrace *> manyTill (declaration <|> invalidStmt) rbrace)

-- | Compare this to the grammar from the book:
--
-- > expression -> assignment ;
-- > assignment -> IDENTIFIER "=" assignment
--                 | equality ;
-- > equality   -> comparison ( ( "!=" | "==" ) comparison )* ;
-- > comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- > term       -> factor ( ( "-" | "+" ) factor )* ;
-- > factor     -> unary ( ( "/" | "*" ) unary )* ;
-- > unary      -> ( "!" | "-" ) unary
-- >               | primary ;
-- > primary    -> NUMBER | STRING | "true" | "false" | "nil"
-- >               | "(" expression ")" ;
expression :: Parser Expr
expression = assignment
  where
    assignment =
      do
        e <- equality
        ( do
            value <- equal *> assignment
            case e of
              Variable t@(Token {tokenType = Identifier _}) -> pure $ Assign t value
              _ -> fail "Invalid assignment"
          )
          <|> pure e
    equality = comparison `chainl1` (bangEqual' <|> equalEqual') -- X ( op X )* => X `chainl1` op
    comparison = term `chainl1` (greater' <|> greaterEqual' <|> less' <|> lessEqual')
    term = factor `chainl1` (minus' <|> plus')
    factor = unary `chainl1` (slash' <|> star')
    unary = (Unary <$> (bang <|> minus) <*> unary) <|> primary
    primary = number <|> stringP <|> bool <|> nil <|> grouping <|> (Variable <$> identifierTok)

parseExpr :: [Token] -> Either [P.LoxParseError] Expr
parseExpr source = case parse expression "" source of
  Left err -> Left [P.LoxParseError (show err) Nothing]
  Right x -> Right x

parseProgram :: [Token] -> Either [P.LoxParseError] [Stmt]
parseProgram source = case parse program "" source of
  Left err -> Left [P.LoxParseError (show err) Nothing]
  Right x ->
    let (invalids, valids) = partition isInvalidStmt x
     in if null invalids
          then Right valids
          else Left $ map extractError invalids
    where
      isInvalidStmt (InvalidStmt _) = True
      isInvalidStmt _ = False
      extractError (InvalidStmt (P.LoxParseError err _)) = P.LoxParseError err Nothing
      extractError _ = error "This should never happen"

-- For debugging only
helper :: T.Text -> Either [P.LoxParseError] [Stmt]
helper source = let (Right t) = scanner source in parseProgram t

-- token parsers, not interesting

slash' :: Parser (Expr -> Expr -> Expr)
slash' = flip Binary <$> slash

star' :: Parser (Expr -> Expr -> Expr)
star' = flip Binary <$> star

bangEqual' :: Parser (Expr -> Expr -> Expr)
bangEqual' = flip Binary <$> bangEqual

equalEqual' :: Parser (Expr -> Expr -> Expr)
equalEqual' = flip Binary <$> equalEqual

greater' :: Parser (Expr -> Expr -> Expr)
greater' = flip Binary <$> greater

greaterEqual' :: Parser (Expr -> Expr -> Expr)
greaterEqual' = flip Binary <$> greaterEqual

less' :: Parser (Expr -> Expr -> Expr)
less' = flip Binary <$> less

lessEqual' :: Parser (Expr -> Expr -> Expr)
lessEqual' = flip Binary <$> lessEqual

minus' :: Parser (Expr -> Expr -> Expr)
minus' = flip Binary <$> minus

plus' :: Parser (Expr -> Expr -> Expr)
plus' = flip Binary <$> plus

number :: Parser Expr
number = token showNumber posFromTok testTok
  where
    showNumber = show
    testTok (Token {tokenType = Number n}) = Just $ Literal (NumberValue n)
    testTok _ = Nothing

stringP :: Parser Expr
stringP = token show posFromTok testTok
  where
    testTok (Token {tokenType = String s}) = Just $ Literal (StringValue $ T.unpack s)
    testTok _ = Nothing

bool :: Parser Expr
bool = token show posFromTok testTok
  where
    testTok (Token {tokenType = TrueTok}) = Just $ Literal (BoolValue True)
    testTok (Token {tokenType = FalseTok}) = Just $ Literal (BoolValue False)
    testTok _ = Nothing

nil :: Parser Expr
nil = token show posFromTok testTok
  where
    testTok (Token {tokenType = Nil}) = Just $ Literal NilValue
    testTok _ = Nothing

grouping :: Parser Expr
grouping = Grouping <$> (lparen *> expression <* rparen)

lparen :: Parser ()
lparen = token show posFromTok testTok
  where
    testTok (Token {tokenType = LeftParen}) = Just ()
    testTok _ = Nothing

rparen :: Parser ()
rparen = token show posFromTok testTok
  where
    testTok (Token {tokenType = RightParen}) = Just ()
    testTok _ = Nothing

bang :: Parser Token
bang = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Bang}) = Just t
    testTok _ = Nothing

minus :: Parser Token
minus = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Minus}) = Just t
    testTok _ = Nothing

plus :: Parser Token
plus = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Plus}) = Just t
    testTok _ = Nothing

slash :: Parser Token
slash = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Slash}) = Just t
    testTok _ = Nothing

star :: Parser Token
star = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Star}) = Just t
    testTok _ = Nothing

greater :: Parser Token
greater = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Greater}) = Just t
    testTok _ = Nothing

greaterEqual :: Parser Token
greaterEqual = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = GreaterEqual}) = Just t
    testTok _ = Nothing

less :: Parser Token
less = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Less}) = Just t
    testTok _ = Nothing

lessEqual :: Parser Token
lessEqual = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = LessEqual}) = Just t
    testTok _ = Nothing

bangEqual :: Parser Token
bangEqual = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = BangEqual}) = Just t
    testTok _ = Nothing

printTok :: Parser Token
printTok = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = PrintTok}) = Just t
    testTok _ = Nothing

semicolon :: Parser Token
semicolon = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Semicolon}) = Just t
    testTok _ = Nothing

notSemicolon :: Parser Token
notSemicolon = token show posFromTok testTok
  where
    testTok (Token {tokenType = Semicolon}) = Nothing
    testTok t = Just t

varTok :: Parser Token
varTok = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Var}) = Just t
    testTok _ = Nothing

identifierTok :: Parser Token
identifierTok = token show posFromTok testTok
  where
    testTok tok@(Token {tokenType = Identifier _}) = Just tok
    testTok _ = Nothing

equal :: Parser Token
equal = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = Equal}) = Just t
    testTok _ = Nothing

eof' :: Parser Token
eof' = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = EOF}) = Just t
    testTok _ = Nothing

equalEqual :: Parser Token
equalEqual = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = EqualEqual}) = Just t
    testTok _ = Nothing

lbrace :: Parser Token
lbrace = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = LeftBrace}) = Just t
    testTok _ = Nothing

rbrace :: Parser Token
rbrace = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = RightBrace}) = Just t
    testTok _ = Nothing

posFromTok :: Token -> SourcePos
posFromTok (Token {line = l, column = c}) = newPos "" l c
