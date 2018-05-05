# obabble
An OCaml Chatbot

Usage
=====

Run `make`.

Then run `./obabble.byte`.

This will use `movie_lines.txt` to generate a `movie_lines.mc` corpus, if it does not exist already; if it already exists, then it loads it from the file.

Once ready, oBabble will open a prompt. Type something and press ENTER to engage in a conversation!

We have some ...surprises... in this repository. More on these during our presentation :)

ACKNOWLEDGEMENTS: Movie lines were sourced from the [Cornell Moive-Dialogs Corpus](http://www.cs.cornell.edu/~cristian/Cornell_Movie-Dialogs_Corpus.html). This is a very extensive corpus---thanks to them for that!

WARNING: The Movie-Dialogs corpus contains lines from many movies, including R-rated ones. Some of the lines are profane---when o[B]abble is trained on these, it may also produce profane results.
