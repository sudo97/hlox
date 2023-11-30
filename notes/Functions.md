# Functions

The book introduces an interface called LoxCallable. I have not yet implemented this. Currently, I have a `call` function that accepts a LiteralValue and either calls a function or throws an error. I may revisit this design decision in the future.

I have introduced a `clock` function that creates a new runtime type `Time`, which internally holds `UTCTime`. This allows for subtraction. For now, it is hardcoded in the eval function, as I want to avoid complex type definitions like `(forall a. LoxCallable a => a)`. If a simpler solution presents itself in the future, I will consider it.
