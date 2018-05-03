# obabble
An OCaml Chatbot

Usage
=====

Run `make`.

Then run `./obabble.byte`.

This will use `movie_lines.txt` to generate a `movie_lines.mc` corpus, if it does not exist already; if it already exists, then it loads it from the file.

Once ready, oBabble will open a prompt. This is just a proof-of-concept right now, and it only accepts one-word seeds. This will soon be able to generate relevant sentences.

Note: Files that end in `.ml_` are experimental extensions that are named as such so that they don't get built my the very zealous Makefile.

TODO:
+ DONE: Unmodule things that don't need modules
+ DONE: Conversational support
* Code cleanup, structure refactoring, and style checks.
* Chatroom
* Personalities, Different corpuses
