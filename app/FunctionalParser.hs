module FunctionalParser where

import AST (Expr (..), LiteralValue (..))
import qualified Data.Text as T
import IdiomaticScanner (Token (..), scanner)
import qualified Parser as P
import Scanner (TokenType (..))
import Text.Parsec
import Text.Parsec.Pos (newPos)

type Parser a = Parsec [Token] () a

-- | Compare this to the grammar from the book:
--
-- > expression     → equality ;
-- > equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- > comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- > term           → factor ( ( "-" | "+" ) factor )* ;
-- > factor         → unary ( ( "/" | "*" ) unary )* ;
-- > unary          → ( "!" | "-" ) unary
-- >               | primary ;
-- > primary        → NUMBER | STRING | "true" | "false" | "nil"
-- >               | "(" expression ")" ;
expression :: Parser Expr
expression = equality
  where
    equality = comparison `chainl1` (bangEqual' <|> equalEqual') -- X ( op X )* => X `chainl1` op
    comparison = term `chainl1` (greater' <|> greaterEqual' <|> less' <|> lessEqual')
    term = factor `chainl1` (minus' <|> plus')
    factor = unary `chainl1` (slash' <|> star')
    unary = (Unary <$> (bang <|> minus) <*> unary) <|> primary
    primary = number <|> stringP <|> bool <|> nil <|> grouping

parseExpr :: [Token] -> Either [P.LoxParseError] Expr
parseExpr source = case parse expression "" source of
  Left err -> Left [P.LoxParseError (show err) Nothing]
  Right x -> Right x

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

equalEqual :: Parser Token
equalEqual = token show posFromTok testTok
  where
    testTok t@(Token {tokenType = EqualEqual}) = Just t
    testTok _ = Nothing

posFromTok :: Token -> SourcePos
posFromTok (Token {line = l, column = c}) = newPos "" l c

-- For debugging only
helper :: T.Text -> Either [P.LoxParseError] Expr
helper source = let (Right t) = scanner source in parseExpr t