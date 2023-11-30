# Functions

The book introduces an interface called LoxCallable. I have not yet implemented this. Currently, I have a `call` function that accepts a LiteralValue and either calls a function or throws an error. I may revisit this design decision in the future.

I have introduced a `clock` function that creates a new runtime type `Time`, which internally holds `UTCTime`. This allows for subtraction. For now, it is hardcoded in the eval function, as I want to avoid complex type definitions like `(forall a. LoxCallable a => a)`. If a simpler solution presents itself in the future, I will consider it.

# Closures

For closures I went with whatever worked...

```hs
 oldEnv <- get -- Capture env as it is before executing function
  put (M.fromList (zip vars' args) : closure) -- recreate env as it was when function was declared + assign function params
  result <- -- Run function and collect the result
    (traverse runStmt body $> NilValue) `catchError` \case
      HackyReturnValue value -> do
        pure value
      e -> throwError e
  envAfterExec <- get -- Capture env after executing the function(the one that was stored as closure, but function could have modified it)
  put oldEnv -- Get back to the oldEnv, since we are done with the function
  modify $ setVariable functionName (Parser.Fun vars body envAfterExec) -- Function is an immutable object, so we need to manually re-save the function in our current scope, now it has the same code, but new closure
  pure result
```

This is not really elegant, I would say. But I don't know any better way. Interesting observation. Next chapter says that the following code should break:

```lox
var a = "global";
{
  fun showA() {
    print a;
  }

  showA(); // In the book this prints "global"
  var a = "block";
  showA(); // and this prints "block"
}
```

But because I didn't (and wan't able to) follow Java implementation that closely, mine works differently. Mine prints "global" twice.
