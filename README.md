levenfind
=========

A tool to find pairs of similar files, typically for checking that your students don't cheat. It works on text files so it should be pretty much agnostic with respect to the language used in the files (be it natural language or code). For now, we use the [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance) (aka _edit distance_) in order to compare contents.

It takes all the file in a directory (by default the current one) and shows all pairs of files whose similarity is above a given threshold (60% by default), the *similarity* being the complement of the edit distance over the length of the files. The algorithm is quadratic, don't be surprised if it takes some time on directories with a few files, especially if some of those are big, although it works fine in practice.

## Usage

```bash
levenfind directory
```

Useful options include

- `--extension`: specify the extension of files to consider,
- `--lines`: compare files line by line instead of character by character (this is much faster, but will consider slightly different lines as distinct),
- `--threshold`: specify the threshold of above which similar files should be reported.
