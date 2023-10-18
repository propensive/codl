1.  A CoDL _document_ is a text document encoded in UTF-8.
2.  A _line_ is a sequence of zero or more characters beginning at the start of the document or the character
    immediately after a newline (`\u000a`) and ending at the end of the document or the character immediately before a
    newline character.
4.  A _data line_ is any line which contains any characters other than space (` `, `\u0020`), unless it is a comment
    line (see below).
5.  The _prefix_ of a data line is the space characters before the first non-space character.
6.  The _initial line_ is the first data line.
7.  The _initial prefix_ is the prefix of the initial line.
8.  Every data line must have a prefix length greater than or equal to the initial prefix length.
9.  The result of subtracting the initial prefix length from the prefix of every line must be an integer multiple of
    two.
10. The _indentation_ of a data line is one half of the result of subtracting the initial prefix from the prefix.
11. The indentation of a data line may be at most two greater than the indentation of the previous line.
12. The _data_ is the the series of characters of a data line after the prefix.
13. The _words_ of a data line are one or more sequences of non-space characters separated by one or more space
    characters.
14. The _keyword_ is the first word of a data line.
15. The _parameters_ are the zero or more words following the keyword which are not comments (see below).
16. The keyword and parameters declare a _node_.
17. The initial line declares the _root node_.
18. With the exception of the root node, each node has a _parent_ and is correspondingly a _child_ of that node.
19. The parent, _p_, of a node other than the root node, _n_, is declared on the last line preceding _n_ whose
    indentation is exactly one less than _n_'s indentation.
20. If the first character after the prefix is a hash (`#`, `\u0023`), that line is a _comment line_ and contains no
    data.

22. If a word is equal to a single hash character, that word and every subsequent word on the same line are a _remark_
    and are not parameters.
23. A trailing 


FIXME: Adjacent comments intented differently
