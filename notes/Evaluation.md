# Evaluation

Currently, evaluation is very straightforward, since we are only evaluating expressions. I'll get back to this note once there's something more interesting, but current implementation is very close to the book and quite idiomatic.

# Update from when I'm halfway through the Statements-And-State chapter

Evaluating expressions that are just arithmetic is one thing. Evaluating statements is a whole other thing. It was easy when I just had prints. But then, this last commit, just look at how much I had to rewrite. The good thing about Haskell is that usually when the change is *big*, *challenging*, or *fundamentally different from what we had*, the compiler shows you exactly how many lines are affected. But in general, it's surprising how many places the process could fail. In a simpler language, it would probably be simpler at first, right? Since we would just have
```js
const globalVars = new Map();

const ast = parse(lex(source));

ast.forEach(stmt => runStmt(ast, globalVars));
```
Right? It would work unless it wouldn't, and you wouldn't be able to capture all the places; you'd have to guess. With Haskell, this guessing is reduced to a minimum. Usually, you express all the obstacles and circumstances with types, and then GHC doesn't let you forget about your initial intentions.
