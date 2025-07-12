# The Collaborative Data Language

## Abstract

CoDL is a language for representing structured and optionally-typed data in
a textual format, for reading and writing by both humans and computers, with
significant whitespace and minimal markup. It facilitates embedding many other
textual languages without escaping, and is particularly well-suited to
line-based diffs. It can be processed fast on modern hardware.

## Conventions

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be
interpreted as described in
[RFC2119](https://datatracker.ietf.org/doc/html/rfc2119) when, and only when,
they appear in all capitals, as shown here.

## Introduction

A CoDL _document_ is a rooted tree of _nodes_. Each tree node holds an ordered
sequence of one or more strings and zero or more ordered child nodes. A
_document_ has a virtual, unique _root_ node which SHOULD have child nodes, but
holds no value.

Each string in the sequence MAY include any Unicode characters, except the first
string which may not contain LINE FEED (code point `U+000A`) or SPACE (code
point `U+0020`), and may not be equal to NUMBER SIGN (code point `U+0035`.

With the exception of multiline strings, the sequence of strings at each node is
written on a single line, following its parent node, before its
sibling nodes, and indented two characters more than its parent.

A _CoDL schema_ specifies how a CoDL document may be interpreted as a rooted
tree of typed values.

A sequence of bytes may be interpreted
as typed or untyped CoDL through a short pipeline.


Bytes -> characters
characters -> lines
lines -> tokens
tokens -> untyped tree
untyped tree -> typed tree



## Specification

### Decoding

A CoDL document MAY be encoded as bytes using either UTF-8 or UTF-16. If UTF-16
is used, the first two bytes MUST be a byte order mark (BOM) of either `0xFF 0xFE` or
`0xFE 0xFF`. The interpretation of bytes as a sequence of characters MUST NOT
include the BOM character at the start of the sequence.

A CoDL document is serialized as a sequence of Unicode codepoints. A document may contain
any Unicode character, including characters outside the ASCII range, outside the
Basic Multilingual Plane and nonprintable characters. However, it is RECOMMENDED
that schemas use only ASCII characters.

A sequence of bytes that does not represent a valid Unicode codepoint MUST produce
an error.

### Line Segmentation

Documents are split into a sequence of _lines_, each of which is a sequence of Unicode
characters. The first line ends immediately before the first occurrence of the
character `LINE FEED` (code point `U+000A`).

Each subsequent line begins immediately after the previous `LINE FEED`
character, and ends immediately before the next `LINE FEED` character. In the
case of two consecutive `LINE FEED` characters, this results in an empty line.

The character `CARRIAGE RETURN` (code point `U+000D`) has no special meaning,
and should be included in the sequence of characters comprising a line.

An empty line or a line containing no character other than `SPACE` (code point
`U+0020`) is called a _whitespace line_. Other lines are called _data lines_,
which are further distinguished as _node lines_ which declare nodes, and _value
lines_ which typically specify values that span multiple lines.

### Tokenization

The first data line in a document MAY contain one or more `SPACE` characters
before the first non-`SPACE` character. These `SPACE` characters are called the
_margin_ of the document, and remains constant for every line. Every data line
in the document MUST NOT start with fewer `SPACE` characters than the margin, or
an error is produced.

It is RECOMMENDED to use an zero-length margin unless the CoDL is embedded
within another language.

Whitespace lines may contain any number of `SPACE` characters.

The _indentation_ of a data line is the number equal to one half of the number
of `SPACE` characters following the margin. The first data line therefore has an
indentation of zero.

The first data line in a document is a _node line_.

Each subsequent data line is determined to be a value line if its indentation is
at least tmo greater than the previous node line.

If a data line has an indentation less than, equal to, or exactly one greater
than the indentation of the previous node line, then it is a node line.

The indentation of a node line MUST be an integer, or an error is produced.

A node line is tokenized as an integer indentation value and a non-empty
sequence of strings. The first token is called the _keyword_, and starts at the
first non-`SPACE` character on the line, and ends immediately before the next
`SPACE` character, or the end of the line.

If the keyword is equal to the one-character string `#`, then the characters after the `SPACE` after the keyword are interpreted as a _comment_.

For any other

### Parsing

### Typing

A CoDL document is serialized as a sequence of Unicode codepoints. A document may contain
any Unicode characters, including nonprintable characters.

A CoDL document MAY be encoded as bytes using either UTF-8 or UTF-16. If UTF-16
is used, the first two bytes MUST be a byte order mark of either `0xFF 0xFE` or
`0xFE 0xFF`


Only three characters have special meaning for tokenization,
 (i)   LINE FEED (code point U+000A)
 (ii)  SPACE (code point U+0020)
 (iii) NUMBER SIGN (code point U+0023)
whereas all others exclusively represent the content of the document and have no
impact ontokenization.

Documents are split into _lines_ by LINE-FEED characters. A line is a sequence of non-LINE-FEED
characters

There is no limit to the length of a line,
or it may be empty.

Each line begins with zero or more SPACE characters. The number of SPACE
characters before the first non-space character is called the _margin_ of that
line. Empty lines and Lines which contain only SPACE characters are _whitespace
lines_. Lines which are not _whitespace lines_ are _data lines_.

The _initial prefix_ is the number of SPACE characters before the first non-SPACE character on the
first data line.

2.  A _line_ is a sequence of zero or more characters beginning at the start of the document or the character
    immediately after a newline (`\u000A`) and ending at the end of the document or the character immediately before a
    newline character.

FIXME: Carriage returns?

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
