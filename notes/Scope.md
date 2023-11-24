# Scope

The book recommends creating a class with a field of its own type to emulate variable scopes. While this is a valid approach, Haskell emphasizes composability. Therefore, I will leverage existing constructs instead of creating new ones. In hlox, the scope is defined as `type Environment = [M.Map T.Text LexicalValue]`. We will then *lift* read/write operations using foldr and other techniques. With the code we've developed, this is all it takes to introduce scope:

```hs
type Environment = [M.Map T.Text LiteralValue]

lookupVariable :: T.Text -> Environment -> Maybe LiteralValue
lookupVariable key = foldr (\env acc -> M.lookup key env <|> acc) Nothing

insertVariable :: T.Text -> LiteralValue -> Environment -> Environment
insertVariable key value [] = [M.singleton key value]
insertVariable key value (env : envs) = M.insert key value env : envs
```

Now, Eval is not just a stateful monad of a single M.Map, but of an Environment. Another minor difference is that we need to modify runStmt to handle blocks. This requires adding a new environment to the stack, traversing each item within it, and then removing the environment after completion.
