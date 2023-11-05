# Parser

So far, I have only implemented the book version of the parser. It uses IdiomaticScanner.

Interestingly, I forgot to append an EOF at the end of my Scanner. I didn't notice this until I tried to parse, because the parser actively uses the 'advance' function, which only increases the pointer when there's room for increase.

I am considering several ways to implement a more interesting parser. I have two ideas:

1. The code turned out to be quite generic. I am thinking of a DSL that would allow me to express it as a data tree. When run, this tree would do the job. The rules are more or less straightforward. You can see them [here](https://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing).
2. Use Parser Combinators that work on the list of tokens. This approach is easier and more common for Haskell.

Frankly, I like idea #1 because I came up with it myself. I like idea #2 because it is likely to be a more viable solution. I am not sure how the book approach is going to evolve, so my generalization could be wrong. I'll revisit this document once I know more. Maybe I'll implement both ideas.
