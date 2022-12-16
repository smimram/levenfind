levenfind
=========

A tool to find pairs of similar files, typically for checking that your students
don't cheat. It works on text files so it should be pretty much agnostic with
respect to the language used in the files (be it natural language or code). For
now, we use the [Levenshtein
distance](https://en.wikipedia.org/wiki/Levenshtein_distance) in order to
compare contents.

It takes all the file in a directory (by default the current one) and shows all
pairs of files whose similarity is above a given threshold (60% by default). The
algorithm is quadratic, don't be surprised if it takes some times on directories
with a few files, especially if some of those are big.
