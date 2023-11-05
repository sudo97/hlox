{-# LANGUAGE OverloadedStrings #-}

module AST where

import qualified Data.Text as T
import IdiomaticScanner (Token (..))

data Expr
  = Binary Expr Token Expr
  | Grouping Expr
  | Literal LiteralValue
  | Unary Token Expr
  deriving (Show)

data LiteralValue
  = StringValue String
  | NumberValue Double
  | BoolValue Bool
  | NilValue
  deriving (Show)

printLiteral :: LiteralValue -> T.Text
printLiteral (StringValue value) = T.pack $ show value
printLiteral (NumberValue value) = T.pack $ show value
printLiteral (BoolValue value) = T.pack $ show value
printLiteral NilValue = "nil"

printExpr :: Expr -> T.Text
printExpr (Binary left (Token {tokenType = operator}) right) = "(" <> T.pack (show operator) <> " " <> printExpr left <> " " <> printExpr right <> ")"
printExpr (Grouping expr) = "(group " <> printExpr expr <> ")"
printExpr (Literal value) = printLiteral value
printExpr (Unary (Token {tokenType = operator}) expr) = "(" <> T.pack (show operator) <> " " <> printExpr expr <> ")"