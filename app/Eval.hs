{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Eval where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (for_, traverse_)
import Data.Functor (($>))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (diffUTCTime, getCurrentTime)
import IdiomaticScanner (Token (..))
import Parser
import Scanner (TokenType (..))

type Environment = [M.Map T.Text LiteralValue]

lookupVariable :: T.Text -> Environment -> Maybe LiteralValue
lookupVariable key = foldr (\env acc -> M.lookup key env <|> acc) Nothing

insertVariable :: T.Text -> LiteralValue -> Environment -> Environment
insertVariable key value [] = [M.singleton key value]
insertVariable key value (env : envs) = M.insert key value env : envs

setVariable :: T.Text -> LiteralValue -> Environment -> Environment
setVariable _ _ [] = []
setVariable key value (env : envs) = case M.lookup key env of
  Nothing -> env : setVariable key value envs
  Just _ -> M.insert key value env : envs

type Eval a = StateT Environment (ExceptT RuntimeError IO) a

runEval :: Eval a -> IO (Either RuntimeError a)
runEval = runExceptT . flip evalStateT []

runStmt :: Stmt -> Eval ()
runStmt (InvalidStmt e) = throwError $ RuntimeError ("This should never happen, some unhandled parsing error, this should actually have happened in the earlier stage" <> show e) (Token {tokenType = EOF, line = 0, column = 0})
runStmt (Print expr) = do
  value <- evaluate expr
  liftIO $ TIO.putStrLn $ printLiteral value
runStmt (Expression expr) = do
  _ <- evaluate expr
  pure ()
runStmt (VarDecl (Token {tokenType = Identifier name}) expr) = do
  value <- evaluate expr
  modify $ insertVariable name value
runStmt (VarDecl _ _) = error "This should never happen, some unhandled parsing error, this should actually have happened in the earlier stage"
runStmt (Block stmts) = do
  modify (M.empty :)
  traverse_ runStmt stmts
  modify tail
runStmt (Parser.If condition thenBranch elseBranch) = do
  condition' <- evaluate condition
  if isTruthy condition'
    then runStmt thenBranch
    else for_ elseBranch runStmt
runStmt s@(Parser.While condition body) = do
  condition' <- evaluate condition
  when (isTruthy condition') $ do
    runStmt body
    runStmt s

evaluate :: Expr -> Eval LiteralValue
evaluate (Literal v) = pure v
evaluate (Variable tok@(Token {tokenType = name})) = case name of
  (Identifier n) -> do
    context <- get
    case lookupVariable n context of
      Just value -> pure value
      Nothing -> throwError $ RuntimeError ("Undefined variable '" <> T.unpack n <> "'") tok
  _ -> throwError $ RuntimeError "It was parsed as variable, but doesn't hold Identifier token, something is very wrong" tok
evaluate (Grouping expr) = evaluate expr
evaluate (Unary tok@(Token {tokenType = operator}) expr) = case operator of
  Minus -> do
    expr' <- evaluate expr
    case expr' of
      (NumberValue value) -> pure $ NumberValue (-value)
      _ -> throwError $ RuntimeError "Operand must be a number" tok
  Bang -> BoolValue . not . isTruthy <$> evaluate expr
  _ -> throwError $ RuntimeError "Unknown unary operator, this is likely a parser error" tok
evaluate (Binary leftExpr tok@(Token {tokenType = operator}) rightExpr) = do
  left <- evaluate leftExpr
  right <- evaluate rightExpr
  case operator of
    Minus -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ NumberValue (n - n')
      (Time t, Time t') -> pure . NumberValue . fromIntegral @Integer . round $ diffUTCTime t t'
      _ -> throwError $ RuntimeError "Operands must be numbers" tok
    Slash -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ NumberValue (n / n')
      _ -> throwError $ RuntimeError "Operands must be numbers" tok
    Star -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ NumberValue (n * n')
      _ -> throwError $ RuntimeError "Operands must be numbers" tok
    Plus -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ NumberValue (n + n')
      (StringValue s, StringValue s') -> pure $ StringValue (s <> s')
      _ -> throwError $ RuntimeError "Operands must be two numbers or two strings" tok
    Greater -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ BoolValue (n > n')
      _ -> throwError $ RuntimeError "Operands must be numbers" tok
    GreaterEqual -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ BoolValue (n >= n')
      _ -> throwError $ RuntimeError "Operands must be numbers" tok
    Less -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ BoolValue (n < n')
      _ -> throwError $ RuntimeError "Operands must be numbers" tok
    LessEqual -> case (left, right) of
      (NumberValue n, NumberValue n') -> pure $ BoolValue (n <= n')
      _ -> throwError $ RuntimeError "Operands must be numbers" tok
    Equal -> pure $ BoolValue (left == right)
    BangEqual -> pure $ BoolValue (left /= right)
    EqualEqual -> pure $ BoolValue (left == right)
    _ -> throwError $ RuntimeError "Unknown binary operator, this is likely a parser error" tok
evaluate (Assign (Token {tokenType = Identifier name}) expr) = do
  value <- evaluate expr
  currentValue <- gets (lookupVariable name)
  case currentValue of
    Nothing ->
      throwError $
        RuntimeError ("Undefined variable '" <> T.unpack name <> "'") (Token {tokenType = EOF, line = 0, column = 0})
    Just _ -> modify (setVariable name value) $> value
evaluate (Assign _ _) =
  error "This should never happen, some unhandled parsing error, this should actually have happened in the earlier stage"
evaluate (Logical leftExpr tok@(Token {tokenType = operator}) rightExpr) = do
  left <- evaluate leftExpr
  case operator of
    Or -> if isTruthy left then pure left else evaluate rightExpr
    And -> if isTruthy left then evaluate rightExpr else pure left
    _ -> throwError $ RuntimeError "Unknown logical operator, this is likely a parser error. " tok
evaluate (Call callee rparen args) = do
  when (length args >= 255) $ -- NOTE: The book does this during parsing, but only warns user, and keeps going, I don't wanna do that in the parser
    throwError $
      RuntimeError "Cannot have more than 255 arguments" rparen
  case callee of
    Variable (Token {tokenType = Identifier "clock"}) -> clock
    _ -> do
      callee' <- evaluate callee
      args' <- traverse evaluate args
      call callee' args' rparen

clock :: Eval LiteralValue
clock = Time <$> liftIO getCurrentTime

call :: LiteralValue -> [LiteralValue] -> Token -> Eval LiteralValue
call ((Parser.Fun vars body)) args tok = do
  when (length args /= length vars) $
    throwError $
      RuntimeError
        ( "Wrong number of arguments. Expected: "
            <> show (length vars)
            <> ", recieved: "
            <> show (length args)
        )
        tok
  modify ((M.fromList $ zip vars args) :)
  traverse_ runStmt body
  pure NilValue
call _ _ tok = throwError $ RuntimeError "Non-callable value" tok

isTruthy :: LiteralValue -> Bool
isTruthy (BoolValue value) = value
isTruthy NilValue = False
isTruthy _ = True

printLiteral :: LiteralValue -> T.Text
printLiteral (StringValue value) = T.pack value
printLiteral (NumberValue value) = T.pack $ show value
printLiteral (BoolValue True) = "true"
printLiteral (BoolValue False) = "false"
printLiteral NilValue = "nil"
printLiteral (Time t) = T.pack $ show t
printLiteral (Parser.Fun expr _) = "fun(" <> T.intercalate ", " expr <> ") { ... }"

printExpr :: Expr -> T.Text
printExpr (Binary left (Token {tokenType = operator}) right) = "(" <> T.pack (show operator) <> " " <> printExpr left <> " " <> printExpr right <> ")"
printExpr (Grouping expr) = "(group " <> printExpr expr <> ")"
printExpr (Literal value) = printLiteral value
printExpr (Unary (Token {tokenType = operator}) expr) = "(" <> T.pack (show operator) <> " " <> printExpr expr <> ")"
printExpr (Variable (Token {tokenType = Identifier name})) = name
printExpr (Variable _) = error "This should never happen, some unhandled parsing error, this should actually have happened in the earlier stage"
printExpr (Assign (Token {tokenType = Identifier name}) expr) = "(set " <> name <> " " <> printExpr expr <> ")"
printExpr (Assign _ _) = error "This should never happen, some unhandled parsing error, this should actually have happened in the earlier stage"
printExpr (Logical left (Token {tokenType = operator}) right) = "(" <> T.pack (show operator) <> " " <> printExpr left <> " " <> printExpr right <> ")"
printExpr (Call callee _ args) = "(" <> printExpr callee <> " " <> T.intercalate " " (printExpr <$> args) <> ")"