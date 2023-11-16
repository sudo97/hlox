# Parser

So far, I have only implemented the book version of the parser. It uses IdiomaticScanner.

Interestingly, I forgot to append an EOF at the end of my Scanner. I didn't notice this until I tried to parse, because the parser actively uses the 'advance' function, which only increases the pointer when there's room for increase.

I am considering several ways to implement a more interesting parser. I have two ideas:

1. The code turned out to be quite generic. I am thinking of a DSL that would allow me to express it as a data tree. When run, this tree would do the job. The rules are more or less straightforward. You can see them [here](https://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing).
2. Use Parser Combinators that work on the list of tokens. This approach is easier and more common for Haskell.

Frankly, I like idea #1 because I came up with it myself. I like idea #2 because it is likely to be a more viable solution. I am not sure how the book approach is going to evolve, so my generalization could be wrong. I'll revisit this document once I know more. Maybe I'll implement both ideas.


## Parser Combinator approach (Next day update)

I decided to write functional parser, since it's very common for Haskell. you can see how the grammar almost mirrors the code. I am still thinking about building a data-structure to represent grammar

## UPD Few days later

My fellow haskellers tell me I should use Parser Combinator and for Synctonization I should use additional variant in AST, special case `ErrorNode` that could be parsed as anything til the end of the statement.

I won't be doing that for now, but will try it later as I go.

## UPD: Implementing Synchronization
First of all, before we talk about synchronization...
I am a lazy person. For the rest of the book, it looks like I'm going to have to keep updating my parser. I am not sure I want to keep updating both implementations, I think what is done is enough for educational purposes. Maybe I will update. The key point is: **It is not guaranteedr**.

Another interesting challenge was with synchronization. The Haskell community suggested I add another AST node, which is now named `InvalidStmt`. I did that. It would be easy if I just wanted to point to the line where the parse error happened. But I wanted a full parsing error. The problem is that the natural way of expressing such things in Parsec is by using Alternative like so:
```hs
program = statement <|> invalidStatement
```
And then if the statement fails, `invalidStatement` is a parser that accepts any token until it reaches `;` thus knowing that it is *ok* to keep parsing after that. `invalidStatement` doesn't know about the previous failure. So I was looking for a way to pass it somehow. The idea was the following:
```hs
program = statement `catchError` invalidStatement
    where
        invalidStatement e = (many notSemicolon *> semicolon) $> InvalidStmt e
```
Since `Parsec` has an instance of `MonadError`, I thought it would work. But it turns out it is not entirely true. The truth is the following:
```hs
instance MonadError e m => MonadError e (ParsecT s u m)
```
And since `Parsec s u` is `Parsec s u Identity`, we cannot have `catchError`(Go implement MonadError for Identity, will it make sense?). It **just doesn't work**. So since I don't care about performance and I am trying to get a grasp at as many concepts as possible instead, I decided to go with the most dumb and obvious solution:
```hs
invalidStatement = do
      stm <- (\xs x -> xs ++ [x]) <$> many notSemicolon <*> semicolon
      case parse statement "" stm of
        Left err -> pure $ InvalidStmt (P.LoxParseError (show err) Nothing)
        Right _ -> error "This is impossible"
```
When the statement branch fails, we collect the same invalid statement inside a parser, and run the parser "manually" against what we collected just to catch the error. Theoretically, it is already there, it shouldn't be required to do it, but there's no easy way to pass it. Others say that [**megaparsec**](https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Error.html#t:ParseError) does it already and collects as many parse errors as possible. But come on, I don't want to write a third implementation.
