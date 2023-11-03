# Scanner

## Bookish
The first version of app/Scanner.hs was a copycat of the book's implementation. I used a record type for the state and shoved it into a State Monad. At first, it stopped at the first error (it was StateT over Either), but I later tweaked it to catch as many errors as it could.

The way it works is by moving the index of the current character in the stream, without mutating the source itself. The character at the current position is then checked against some predefined patterns, some of which are one-character long, while others could be infinite.

## The Idiomatic Approach
After a while, I realized that the initial approach was not very Haskell-like. So, I came up with a new version in app/IdiomaticScanner.hs. It still uses a State Monad, but mainly because I was too lazy to pass around the current position in the text as a function argument. Instead of moving an index and accessing characters by it, this version just chops off characters from the source one by one and builds a lazy list of Tokens.

Both the Bookish and this Lazy scanner have the same type, so you can use whichever you prefer.
