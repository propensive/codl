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

The process of interpreting a CoDL _document_ is described as a pipeline of five
stages, which process input as a sequence of bytes:

 1. decoding,     into a sequence of characters
 2. segmentation, into a sequence of lines
 3. tokenization, into a sequence of nodes
 4. parsing,      into a tree of sequences of strings
 5. typing,       into a tree of typed values

Implementations of CoDL interpreters MAY follow this sequence of steps, but
alternative implementations which produce the same data structures are equally
valid.

Implementations SHOULD be able to process input as either a sequence of bytes or
a sequence of characters, and SHOULD produce output structured as a tree of
sequences of strings or a tree of typed values. However, some implementations
MAY omit one input format and/or one output format while remaining useful.

These stages serve as an instructive way to describe the format.

### Decoding

Decoding transforms a sequence of bytes into a sequence of Unicode characters.

A sequence of bytes representing the character sequence of a CoDL document MUST
use either UTF-8 or UTF-16. If UTF-16 is used, the first two bytes MUST be a
byte order mark (BOM) of either `0xFF 0xFE` or `0xFE 0xFF`. The sequence of
characters MUST exclude the BOM character from the start of the sequence.

If the last character of the sequence is not `LINE FEED`, then a `LINE FEED`
character MUST be appended to the sequence of characters.

A document may contain any Unicode character, including characters outside the
ASCII range, outside the Basic Multilingual Plane and nonprintable characters.
However, it is RECOMMENDED that documents only include.

A sequence of bytes that does not represent a valid Unicode codepoint MUST produce
an error.

### Segmentation

Segmentation transforms a sequence of characters into a sequence of _lines_.

Documents are split into a sequence of _lines_, each of which is a sequence of
Unicode characters. The first line ends immediately before the first occurrence
of the character `LINE FEED` (code point `U+000A`).

Each subsequent line begins immediately after the previous `LINE FEED`
character, and ends immediately before the next `LINE FEED` character. In the
case of two consecutive `LINE FEED` characters, this results in an empty line.

The character `CARRIAGE RETURN` (code point `U+000D`) has no special meaning,
and should be included in the sequence of characters comprising a line.

An empty line or a line containing no character other than `SPACE` (code point
`U+0020`) is called a _whitespace line_. Other lines are called _content lines_.

The first line MAY begin with the character sequence, `NUMBER SIGN`,
`EXCLAMATION MARK` (code points `U+0023`, `U+0021`). In such case, the remainder
of the line following these two characters is captured as a _shebang command_,
and the entire line is omitted from the output sequence of lines, and hereafter,
_first line_ refers to the line immediately following the shebang command.

### Tokenization

The first content line in a document MAY contain one or more `SPACE` characters
before the first non-`SPACE` character. These number of `SPACE` characters is
called the _margin_ of the document. The margin remains constant for every line.
Every content line in the document MUST NOT start with fewer `SPACE` characters
than the margin, or an error is produced.

It is RECOMMENDED to use a margin of zero unless the CoDL is embedded within
another language.

Whitespace lines may be empty or contain any number of `SPACE` characters.

The _indentation_ of a content line is the number equal to one half of the number
of consecutive `SPACE` characters following the margin. The first content line
therefore has an indentation of zero.

The first content line in a document is a _node line_.

Each subsequent content line is determined to be a _value line_ if its
indentation is at least tmo greater than the previous node line. The content in
a value line is the sequence of characters of the _content line_ with the first
_n_ `SPACE` characters removed, where _n_ is given by,

   n = margin + 2*indentation + 4

If a content line has an indentation less than the previous node line, equal to
the previous node line, or exactly one greater than the previous node line, then
it is a node line.

The indentation of a node line MUST be an integer, or an error is produced.

A node line is tokenized as an integer indentation, a _keyword_ and zero or more _parameters_.
The first token is called the _keyword_, and starts at the
first non-`SPACE` character on the line, and ends immediately before the next
`SPACE` character, or the end of the line.

If the keyword is equal to the one-character string `#`, then the characters
after the `SPACE` after the keyword are a _comment_ which begins at the first
non-`SPACE` character following the keyword, `#`, and continues to the end of
the line.

For any other keyword,

### Parsing
  The node lines are processed in order.


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
lines_. Lines which are not _whitespace lines_ are _content lines_.

The _initial prefix_ is the number of SPACE characters before the first non-SPACE character on the
first content line.

2.  A _line_ is a sequence of zero or more characters beginning at the start of the document or the character
    immediately after a newline (`\u000A`) and ending at the end of the document or the character immediately before a
    newline character.

FIXME: Carriage returns?

4.  A _content line_ is any line which contains any characters other than space (` `, `\u0020`), unless it is a comment
    line (see below).

5.  The _prefix_ of a content line is the space characters before the first non-space character.
