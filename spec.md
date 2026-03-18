# CODL Specification Draft

## Abstract

CODL is a line-oriented, tree-structured data language designed for data that is read, written and
maintained by both humans and machines.

CODL defines a **presentation model** that faithfully preserves formatting, comments and document
structure through programmatic round-trips, and a schema-driven **semantic model** that ascribes
types to every node in the tree. The two models are connected by a deterministic type-assignment
algorithm, and a compact binary encoding (BCODL) provides an unambiguous serialization of the
semantic model.

The design of CODL is motivated by the following goals:

- **Formatting preservation.** Machine edits should not disturb formatting, comments or whitespace
  on lines they do not semantically change, so that line-based version control produces minimal,
  meaningful diffs.
- **Minimal escaping.** Syntax conflicts between content and structure should be rare; literal and
  source atoms allow arbitrary content with no character escaping.
- **Strict, recoverable parsing.** Parsing is unambiguous and every error condition has a defined
  recovery strategy, so that a single mistake does not shadow subsequent errors.
- **Schema-driven typing.** Every node is typed by a schema. Validation, including string-level
  constraints, is an integral part of the format rather than an external layer.
- **Layered extensibility.** Schemas support append-only layering, enabling forwards-compatible
  extensions with clear compatibility semantics.
- **Human and machine editors.** The format is designed for direct human authorship, IDE tooling
  with immediate feedback, programmatic transformation, and AI-assisted editing alike.

## 1. Status

This document is a draft specification of CODL.

Where this draft contains `FIXME` notes, the corresponding behavior is not yet fully specified and
MUST NOT be considered stable.

## 2. Conformance Language

The key words **MUST**, **MUST NOT**, **REQUIRED**, **SHALL**, **SHALL NOT**, **SHOULD**, **SHOULD
NOT**, **RECOMMENDED**, **MAY**, and **OPTIONAL** in this document are to be interpreted as
described in RFC 2119 and RFC 8174 when, and only when, they appear in all capitals.

## 3. Overview

CODL is a Unicode, character-based language for ordered, tree-structured data represented as
strings, and typed according to a schema.

CODL presents data as an _ordered_ tree, however an application consuming CODL MAY choose to assign
meaning to sibling order, or MAY treat it as insignificant. In this respect, CODL is similar to XML.

CODL distinguishes between:

- a **presentation model**, which preserves comments, interpreter directives, pragma metadata, atom
  presentation form, most whitespace and document structure sufficiently for faithful
  reserialization, and
- a **semantic model**, which is derived from the presentation model using a schema.

This document specifies CODL source, its parsing into the presentation model, the definiton of
schemas and translation between presentation model and semantic model by means of a schema.

## 4. Character Encoding

CODL is defined over Unicode code points.

When serialized as binary data, a CODL document MUST be encoded as UTF-8.

CODL uses `U+000A` LINE FEED (`LF`) as its primary line-ending character. Carriage return (`CR`,
`U+000D`) is also permitted under the following rules.

A `CR` appearing anywhere in a CODL document outside a literal atom payload MUST be immediately
followed by `LF` (**E123**).

The **line-ending mode** of a document is determined by the first `LF` character in the document:

- if that `LF` is immediately preceded by `CR`, the mode is **CRLF mode**
- otherwise, the mode is **LF mode**

Once the mode is established, every subsequent `LF` outside a literal atom payload MUST conform to
it: in CRLF mode every `LF` MUST be preceded by `CR`; in LF mode `CR` MUST NOT appear before any
`LF`. A violation of this rule is also a **E123** error.

If the document contains no `LF` characters outside literal atom payloads, no mode is established
and no E123 errors can arise.

No Unicode normalization is required or implied. CODL is defined over the exact Unicode code points
that appear in the serialized text.

A UTF-8 byte order mark MUST NOT appear in a CODL document (**E101**).

Visually misleading code points, such as zero-width characters, SHOULD be avoided. Control-heavy
content SHOULD be avoided except where required. CODL is not intended primarily as a binary-data
format, even though it can represent content containing non-printing code points.

## 5. Significant Characters and Terms

The following characters have syntactic significance in CODL:

- `U+000A` LINE FEED (`LF`)
- `U+0020` SPACE
- the sigil (§8.3)

A **line** is a contiguous, potentially empty sequence of non-linefeed characters delimited by
linefeed characters or by the start or end of the file.

A **soft space** is exactly one `U+0020` SPACE character.

A **hard space** is two or more consecutive `U+0020` SPACE characters.

A **blank line** is a line containing only `U+0020` SPACE characters, or no characters at all.

Blank lines have no defined indent.

A **word** is a maximal contiguous sequence of non-newline, non-separator characters on a line,
where separators are determined by the word-separation rules (§10.3).

An **ordinary line** is any non-blank line that is not a comment line (§11.1), a tabulation line
(§11.2), or a line forming part of a source atom (§15) or literal atom (§16) payload.

## 6. Root Structure

A parsed CODL document has the following root structure:

```typescript
interface Document {
  interpreterDirective: string | null;
  pragma: Pragma | null;
  lineEndings: "LF" | "CRLF";
  children: Block[];
}

interface Pragma {
  version: [number, number];
  schema: string | null;
  sigil: Sigil | null;
}

type Sigil =
  | "!"
  | '"'
  | "#"
  | "$"
  | "%"
  | "&"
  | "'"
  | "("
  | ")"
  | "*"
  | "+"
  | ","
  | "-"
  | "."
  | "/"
  | ":"
  | ";"
  | "<"
  | "="
  | ">"
  | "?"
  | "@"
  | "["
  | "\\"
  | "]"
  | "^"
  | "_"
  | "`"
  | "{"
  | "|"
  | "}"
  | "~";
```

## 7. Interpreter Directive

If the first two characters of the document are `#!`, then the first physical line of the document
is an interpreter directive line. If not, the interpreter directive is absent.

The interpreter directive payload is the content of the first line after the leading `#!`, up to but
excluding the terminating newline.

If a document has an interpreter directive and also has a pragma, then the pragma MUST appear after
the interpreter directive.

An interpreter directive line is not part of the `children` sequence.

## 8. Pragma

If present, the pragma MUST be the first non-blank line after any interpreter directive line, and is
parsed using the ordinary CODL line rules (**E102**).

If present, the entire pragma line MUST be fully contained within the first 4096 bytes of the
document (**E103**).

The keyword of the pragma line MUST be `pragma`.

The positional form of the pragma is:

```text
pragma 1.0 schema-id #
```

The parameters are interpreted in order as follows:

1. CODL version
2. schema identifier
3. sigil

The version parameter MUST have the form `x.y`, where `x` and `y` are non-negative integers
(**E104**). `x` is the major version and `y` is the minor version.

The following rules govern how the version number changes across revisions of this specification:

- A revision that accepts documents which would not be accepted by the previous revision MUST
  increment the major version.
- A revision that accepts a previously accepted document but assigns it a different interpretation
  in its presentation or semantic model MUST increment the major version.
- A revision that rejects a document that would have been accepted by an earlier revision MUST keep
  the same major version and increment the minor version.

The schema identifier parameter is optional.

The sigil parameter is optional.

If the sigil parameter is present, the schema identifier parameter MUST also be present (**E105**).

The sigil MUST be a single ASCII symbolic character. It MUST NOT be SPACE, LINEFEED, CARRIAGE
RETURN, a letter, a control character or a digit (**E106**).

The default sigil is `#`, used unless the pragma or the document schema specifies a different one.

### 8.1 Schema Identifier

The schema identifier, if present, MUST be one of:

- an HTTP or HTTPS URL, optionally with a fragment (the `#` separator and everything after it) that
  is the BASE64-URL-encoded (no padding) SHA-256 hash of the BCODL representation of the schema
- a bare BASE64-URL-encoded (no padding) SHA-256 hash of the BCODL representation of the schema

The `#` used in the URL form is the standard URI fragment separator (RFC 3986 §3.5). A bare hash is
distinguished from a URL by the absence of a `://` substring. Because the BASE64-URL alphabet
contains no space characters, a schema identifier always occupies a single word.

The BCODL format and the schema hash derivation are defined in §20.4.

### 8.2 Schema Resolution

A schema may be supplied in two independent ways when parsing a CODL document:

- an **invocation schema**, supplied directly to the parser by the calling application
- a **document schema**, identified by the schema parameter in the pragma

The following table defines the outcome for each combination:

| Invocation schema | Document schema       | Outcome                                                      |
| ----------------- | --------------------- | ------------------------------------------------------------ |
| absent            | absent                | Untyped document; only the presentation model is available   |
| absent            | present               | Semantic model available, but types are not statically known |
| present           | absent                | Semantic model available with statically known types         |
| present           | present, matching     | Same as invocation-only; types are statically known          |
| present           | present, compatible   | Parsed with invocation schema; types are statically known    |
| present           | present, incompatible | Error                                                        |

Two schema identifiers **match** if:

- both carry a hash, and the hashes are identical; or
- neither carries a hash, and the URLs are identical

A schema identifier that carries a hash takes precedence for matching purposes: a URL-only
identifier and a URL-with-hash identifier for the same URL do not automatically match (the hash is
authoritative).

A schema with signature A is **compatible** with a schema with signature B if the decoded hash
sequence of A is a subsequence of the decoded hash sequence of B. That is, A's components (base and
layers) appear in B in the same order, but B may include additional layers between or after them.
Compatibility is directional: if A is compatible with B, a reader expecting schema B can read a
document written against schema A, because B's composed type structure is a superset of A's (layers
are append-only). The converse does not hold: a reader expecting A cannot necessarily read a
document written against B, since B may contain members unknown to A.

If a schema URL is specified but the schema cannot be retrieved, it is a runtime error.

### 8.3 Sigil Resolution

The sigil is determined in the following order of increasing precedence:

1. The default sigil (`#`)
2. The sigil declared by the resolved schema, if any
3. The sigil specified in the pragma, if present

The sigil MUST be determined before parsing any content after the pragma line. If the effective
sigil requires the schema (i.e., the pragma does not specify a sigil and the schema may declare
one), the parser MUST resolve the schema before continuing.

The sigil declared by a schema is given by `Schema.sigil` (§20).

The pragma line is not included in the `Document.children` sequence. It is recorded only in the
`Document.pragma` field.

## 9. Lines, Margin, and Indentation

A CODL document MAY begin with zero or more blank lines.

A document containing no non-blank lines is invalid (**E107**).

If the document begins with an interpreter directive, the **margin** is zero. Otherwise, the
**margin** is the sequence of leading spaces on the first non-blank line.

Every non-blank line in the document MUST begin with at the margin, optionally followed by
additional spaces. A non-blank line which does not begin with the margin is invalid (**E108**).

For each non-blank line, the number of spaces following the margin MUST be even (**E109**). The
**indent** is defined as one half of the number of spaces between the margin and the first non-space
character.

Therefore, after removing the margin, indentation is measured in units of two spaces, and the first
non-blank line necessarily has indent `0`.

Trailing spaces on a non-blank ordinary line are not permitted (**E110**).

Blank lines have no structural effect, except where explicitly stated for tabulated blocks, source
atoms, or literal atoms.

## 10. Keywords and Inline Atoms

Each non-blank ordinary line is parsed into one or more words by the word-separation rule (§10.3).

The first word on a non-blank ordinary line is the **keyword**.

Each subsequent word on that line is an **inline atom**.

### 10.1 Keyword Characters

A keyword may contain any Unicode code point except `U+0020` SPACE and `U+000A` LINE FEED.
Non-printing code points are NOT RECOMMENDED in keywords but are not forbidden. Although non-ASCII
keywords are permitted, ASCII keywords are generally RECOMMENDED for interoperability and
readability.

### 10.2 Inline Atom Characters

An inline atom may contain any Unicode code point other than newline, subject to the CODL
word-separation rules and any special parsing rules for remarks.

### 10.3 Word-Separation Rule

After the keyword, the line is parsed left-to-right.

Initially, a single space is sufficient to terminate a word and begin the next word.

If a hard space is encountered anywhere after the keyword, then from that point onward only hard
spaces terminate words. Soft spaces after that point become part of the current word.

Accordingly:

- before the first hard space on a line, either a soft space or a hard space terminates a word
- after the first hard space on a line, only a hard space terminates a word
- after the first hard space on a line, a soft space becomes content within the current word
- each new line resets this rule

Consequently, after the first hard space on a line, a word may contain soft spaces but may not
contain hard spaces.

The remark rule (§11.3) is subject to the word-separation mode in effect at the point it is tested.
In soft-space mode, the sigil preceded by a soft space is a remark introducer. In hard-space mode,
the sigil preceded by a soft space is not at a word boundary and is therefore not a remark
introducer; it must be preceded by a hard space to be recognized as one.

## 11. Comments, Tabulations, and Remarks

CODL distinguishes between:

- a **comment**, which occupies an entire line and is represented as a line-level presentation node,
- a **tabulation**, which occupies an entire line and represents the start of a tabulated block, and
- a **remark**, which is attached to a compound line and is not an ordinary child node.

The document's **sigil** — the character that introduces comments, tabulations, and remarks — is
determined by the resolution rules in §8.3.

### 11.1 Comment

A line is a comment line if, after its leading indentation, its keyword is exactly equal to the
sigil, and the line does not qualify as a tabulation line (§11.2). A line qualifies as a tabulation
line if at least one further occurrence of the sigil appears on the line preceded by a hard space;
in that case the line is a tabulation line and not a comment line, regardless of any other content.

If the sigil is followed immediately by the end of line, the comment payload is the empty string.

If the sigil is followed by a soft space, the comment payload begins at the first character after
that soft space and continues unchanged to the end of the line.

A word such as `#foo` is never a sigil.

The payload of a comment is not further parsed. Spaces inside the payload are preserved exactly.

Comments participate in indentation and structural ordering as line-level nodes. Comments cannot
have children.

A comment line MUST be immediately preceded by a blank physical line, unless it is the first
non-blank physical line of the children sequence (i.e., the first non-blank physical line after any
interpreter directive and pragma) (**E111**). Because a blank line terminates any active tabulated
block, this rule ensures that comments cannot appear inside tabulated blocks.

A comment is **attached** to the immediately following node if there is no blank line between the
comment and that node. The following node may be a compound node, or a tabulation line (in which
case the comment is attached to the tabulated block that the tabulation line introduces). The
attached node MUST be at the same indentation level as the comment. A comment that is followed by a
blank line, by end of input, or by a line at a shallower indentation level is **free-standing**.

Comment attachment is a semantic property recorded in the presentation model. It is significant
during programmatic editing: when a node is moved or deleted, its attached comments travel with it
or are removed with it.

### 11.2 Tabulation

A line is a tabulation line if, after its leading indentation, its first non-space character is the
sigil, and at least one further occurrence of the sigil appears on the line immediately preceded by
a hard space.

Each marker occurrence on a tabulation line is identified as follows: the first non-space character
(M_0) is always a marker; any subsequent occurrence of the sigil that is immediately preceded by a
hard space is also a marker (M_1, M_2, …).

A tabulation line is represented as a distinct presentation node. The remark rule (§11.3) does not
apply to tabulation lines; any content on a tabulation line that would otherwise form a remark is
instead part of the heading text for the final column.

The markers on a tabulation line are ordered by position. Let their character offsets from the start
of the line, after removing the document margin, be M_0 < M_1 < ... < M_n, where n ≥ 1. The first
marker (at M_0) is the line's keyword and carries no column semantics. Each subsequent marker
defines a column of the tabulated block: marker at M_i (for i = 1, ..., n) defines the start of
**column i**. Columns are numbered from 1.

For each non-final column i (where 1 ≤ i < n), its maximum content width is M\_{i+1} − M_i − 2 code
points. The final column (i = n) is unbounded.

**Column headings.** Each marker M_i is followed by a **column heading**, parsed as follows:

- If M_i is immediately followed by end of line, the heading is the empty string.
- If M*i is immediately followed by exactly one space (a soft space), the heading is the text
  beginning after that space and ending immediately before the first hard space encountered, or at
  end of line if no hard space follows. If the heading ends at a hard space, the character
  immediately after that hard space MUST be the sigil (i.e., M*{i+1}) (**E122**). The heading text
  MUST NOT itself contain the sigil (**E122**).
- If M*i is immediately followed by two or more spaces (a hard space), the character immediately
  after those spaces MUST be the sigil (i.e., M*{i+1}), and the heading for M_i is the empty string
  (**E122** if not).
- Any other character immediately following M_i (including a non-space character) is invalid
  (**E122**).

The column heading for M_0 labels the keyword and pre-column area of rows. The column heading for
M_i (i ≥ 1) labels column i and is positioned within column i's span on the tabulation line.

Column headings are preserved in the `Tabulation` node as an ordered list parallel to
`markerOffsets`. An empty string heading is permitted.

Examples:

- `# ID  # Name  # Age` — three markers; headings `["ID", "Name", "Age"]`
- `#  # Name  # Age` — M_0 followed by hard space then M_1; headings `["", "Name", "Age"]`
- `# ID  #  # Age` — M_1 followed by hard space then M_2; headings `["ID", "", "Age"]`
- `# foo  # # bar` — invalid (**E122**): heading for M_1 would contain the marker
- `# foo  #  bar  # baz` — invalid (**E122**): M_1 followed by hard space not immediately preceding
  a marker

### 11.3 Remark

A remark is attached to the compound defined by its line.

A remark begins when the sigil appears at the start of a word and is immediately followed by exactly
one soft space. Whether a given occurrence of the sigil is at the start of a word depends on the
word-separation mode in effect at that point (§10.3).

Accordingly:

- the sigil followed by end of line is an ordinary word, not a remark introducer
- the sigil followed by a hard space is an ordinary word, not a remark introducer
- the sigil not preceded by a word boundary in the current mode is ordinary content within the
  current word
- the sigil at a word boundary followed by a soft space introduces a remark

The remark payload begins at the first character after that soft space and continues unchanged to
the end of the line.

The sigil itself is not part of the remark payload.

A remark payload is not further parsed.

A compound may have at most one remark.

Remarks do not terminate or split a tabulated block.

## 12. Presentation Nodes

The presentation-layer node types are:

```typescript
interface Compound {
  keyword: string;
  atoms: Atom[];
  remark: string | null;
  children: Block[];
}

interface Block {
  comments: Comment[];
  tabulation: Tabulation | null;
  compounds: Compound[];
  trailingBlankLines: number;
}

interface Comment {
  text: string;
}

interface Tabulation {
  markerOffsets: number[];
  headings: string[];
}

type Atom = Inline | Source | Literal;

interface Inline {
  text: string;
  precedingSpaces: number;
}

interface Source {
  text: string;
}

interface Literal {
  text: string;
}
```

These distinctions are presentation-only.

In the semantic model, all atom presentation forms are just atoms, and the distinction between atom
and compound disappears in favor of typed nodes with types.

A `Block` is the primary structural grouping within a compound's children. It consists of, in order:

- zero or more **attached comments** (each preceded by a blank line, per §11.1)
- an optional **tabulation** line, which applies to all compounds in the block
- zero or more **compound children** (rows if the tabulation is present, ordinary children
  otherwise)
- a count of **trailing blank lines**: the number of blank lines that follow the last compound (or,
  if compounds is empty, the last comment) in the block

A block has at most one tabulation. If a tabulation is present, it MUST appear after any attached
comments and before the first compound child. A block with a tabulation MUST have at least one
compound child (row).

A block whose `compounds` list is empty and whose `comments` list is non-empty represents a
free-standing comment group (a comment or comments not immediately followed by any compound at the
same level). Such a block has no tabulation.

Each `Atom.Inline` records the number of spaces immediately preceding it on its source line.

## 13. Compound Tree Structure

Each non-comment non-tabulation ordinary line defines a `Compound` node whose keyword is the line
keyword.

Each subsequent inline atom after the keyword defines an `Atom.Inline` attached to that compound,
unless superseded by the remark rule.

After its inline atoms, a compound may have zero or more child blocks (§12), determined by
indentation and blank-line structure.

## 14. Parent, Child, and Peer Relations

For each non-blank line after the first non-blank line, excluding lines consumed by source atoms or
literal atoms:

- if its indent is exactly one greater than that of the immediately preceding non-blank line, it is
  a child of the immediately preceding non-blank line
- if its indent is equal to that of the immediately preceding non-blank line, it is a peer of the
  immediately preceding non-blank line
- if its indent is less than that of the immediately preceding non-blank line, it closes one or more
  open compounds and becomes a peer of the nearest preceding line with the same indent; if no
  preceding line has the same indent, the document is invalid (**E112**)

A line may not have indent greater than one plus the indent of the immediately preceding non-blank
line, except where the source-atom or literal-atom rules apply (**E113**).

Comments and tabulations follow the same indentation and peer/child rules as compounds during
parsing, except that comments and tabulations cannot have children. A line that would become a child
of a comment or tabulation is invalid (**E114**). In the resulting presentation model, comments and
tabulations are absorbed into `Block` nodes and do not appear as standalone siblings of compounds.

## 15. Source Atoms

If a line immediately follows a compound line with no intervening blank line, and its indent is
exactly two greater than that compound line's indent, then it begins a source atom, provided:

- the preceding compound does not already have a source atom or literal atom

A source atom is represented in the presentation model as `Atom.Source(text)` and is appended to the
end of the atom sequence of the immediately preceding compound.

A compound may have at most one source atom. Introducing a source atom when the preceding compound
already has a source or literal atom is invalid (**E115**).

The source atom begins on the double-indented line and includes that line together with each
subsequent line until either:

- the end of the document is reached, or
- a non-blank line is encountered whose indent is less than the indent of the first source-atom line

Blank lines are permitted within a source atom.

For each non-blank captured line, exactly the indentation of the first source-atom line is stripped
from the start of the line. Any surplus leading spaces are preserved.

For each captured line, trailing spaces are stripped. (Source-atom lines are not ordinary lines, so
E110 does not apply to them; trailing spaces are silently removed rather than being an error.)

A blank line within a source atom contributes exactly a newline character to the source-atom
payload, regardless of how many spaces it physically contains.

The source-atom payload is otherwise captured literally. In particular, the sigil has no special
meaning inside a source atom.

The source-atom payload always ends with exactly one final newline character.

Source-atom lines are not compounds and are never members of a tabulated block. A source atom always
terminates any surrounding tabulated block.

After a source atom ends, parsing resumes normally. The next non-source-atom line is evaluated for
indentation relative to the compound that introduced the source atom, as if the source atom lines
were not present.

## 16. Literal Atoms

If a line immediately follows a compound line with no intervening blank line, and its indent is
exactly three greater than that compound line's indent, then it begins a literal atom, provided:

- the preceding compound does not already have a source atom or literal atom

A literal atom is represented in the presentation model as `Atom.Literal(text)` and is appended to
the end of the atom sequence of the immediately preceding compound.

A compound may have at most one literal atom. Introducing a literal atom when the preceding compound
already has a source or literal atom is invalid (**E116**).

The opening literal-atom line is not part of the payload.

The remainder of that opening line, from its first content character up to but excluding its
terminating newline, is the delimiter.

The delimiter MUST consist only of ASCII characters other than space and newline.

If the delimiter is empty, the line does not begin a literal atom.

The literal payload begins immediately after the newline terminating the delimiter line.

The payload continues verbatim until a subsequent physical line whose entire line is exactly equal
to the delimiter.

The closing delimiter line MUST be followed by a newline.

The closing delimiter line is not part of the payload.

The single newline immediately preceding the closing delimiter line is also not part of the payload.

Accordingly, an empty literal payload is permitted.

The literal payload preserves leading spaces, trailing spaces, internal spaces, and all other
content exactly.

If the end of file is reached before a closing delimiter line is encountered, the document is
invalid (**E117**).

The sigil has no special meaning inside a literal atom.

The line-ending mode rules of §4 do not apply inside a literal atom payload. `CR` characters within
the payload are preserved exactly as-is and carry no special meaning; only the `LF` that terminates
each payload line is recognised as a line separator.

Literal atom payload lines are raw: they are not subject to any CODL parsing rules. Indentation,
trailing spaces, and all other content are preserved exactly. The only termination condition is the
occurrence of a line whose entire content is exactly equal to the delimiter.

Literal-atom lines are not compounds and are never members of a tabulated block. A literal atom
always terminates any surrounding tabulated block.

After the closing delimiter line and its terminating newline, parsing resumes normally. The next
non-literal-atom line is evaluated for indentation relative to the compound that introduced the
literal atom, as if the literal atom lines were not present.

## 17. Tabulated Blocks

A **tabulated block** begins immediately after a tabulation line and continues through each
subsequent non-blank line until a blank line is encountered or the end of the document is reached.
Lines within a tabulated block (other than the tabulation line itself) are called **rows**.

In the presentation model, a tabulated block is represented as a `Block` whose `tabulation` field
holds the tabulation line and whose `compounds` list holds the rows.

A second tabulation line appearing within a continuous run of rows (without an intervening blank
line) terminates the current `Block` and begins a new `Block` with the new tabulation. The new
block's `trailingBlankLines` on the preceding block is zero, indicating that no blank lines separate
the two tabulated sub-blocks.

Every non-blank, non-comment row MUST be an ordinary compound line. Every row MUST have the same
indent as the tabulation line (**E118**). Rows MUST NOT have child line-nodes (**E114**).

**Row structure.** Each row consists of a keyword and zero or more **pre-column atoms**, followed by
zero or more **column values**. The keyword and pre-column atoms are separated from each other by
single spaces. Column values are introduced by the column positions defined by the tabulation line.

**Spacing constraints.** The following two rules govern the spacing on every row:

1. Every contiguous run of two or more space characters (a hard space) MUST end at position M_i − 1
   for some column i that is present on the row (**E119**).
2. No two consecutive space characters may appear at any other position on the row (that is, within
   the keyword, within pre-column atoms, or within a column value) (**E120**).

These rules together imply:

- the keyword and pre-column atoms are separated from each other by exactly one space
- before each present column i there is exactly one hard space run, ending at M_i − 1
- column values contain no internal consecutive spaces

**Column presence and values.** Column i is **present** on a row if the row contains space
characters at both position M_i − 2 and position M_i − 1 (the mandatory minimum for the hard-space
separator). Column i is **absent** from a row if the row ends before reaching position M_i − 2; a
row need not specify all columns and may omit any suffix of columns.

A present column has an **empty value** if position M_i is itself a space character or the row ends
at position M_i − 1. An empty value requires that the subsequent column (if any) is also present,
since otherwise the separator spaces at M_i − 2 and M_i − 1 would be trailing spaces, which are not
permitted. A row MUST NOT have trailing spaces (**E110**).

**Width constraint.** For each present non-final column i, its value MUST NOT exceed M\_{i+1} − M_i
− 2 code points in width (**E121**). The final column is unbounded.

**Remarks.** Remarks are permitted on rows. The hard space that introduces a remark, and the remark
payload itself, are exempt from the column spacing constraints and are not subject to column-width
limits.

If a row violates any of these constraints, the document is invalid (see **E118** through **E121**).

## 18. Presentation Model and Semantic Model

CODL defines both a presentation model and a semantic model.

### 18.1 Presentation Model

The presentation model is constructed during parsing. When a schema is available, parsing and type
assignment proceed together: the schema informs error recovery decisions (particularly for
indentation errors; see §19.5) that cannot be resolved from syntax alone.

It preserves:

- the optional interpreter directive
- the optional pragma
- compounds and their keywords, atoms, and remarks
- the block structure: for each block, its attached comments, its optional tabulation, its ordered
  compound children, and its trailing blank line count
- atom presentation form (`Inline`, `Source`, or `Literal`)
- for each inline atom, the number of spaces immediately preceding it
- ordering and structure derived from indentation

A conforming serializer MUST produce output that, when parsed, yields an identical presentation
model. Specifically, the serialized output MUST preserve:

- all compounds, with their keywords, atoms, remarks, and children
- all blocks, with their comments, tabulations (including marker offsets and headings), compounds,
  and `trailingBlankLines` counts
- for each `Inline` atom, the `precedingSpaces` count
- document-level fields: interpreter directive, pragma, and children order

A serializer MAY normalize blank line content to empty lines (rather than space-only lines), and MAY
use a minimum hard space (exactly two spaces) before remark introducers, since neither is recorded
in the presentation model. All other presentation-model details MUST be reproduced exactly.

### 18.2 Semantic Model

The semantic model is derived from the presentation model by applying the type assignment algorithm
(§20.2) during parsing. The result is a tree of `Element` values:

```typescript
type Element = Node | Value;

interface Node {
  type: Type;
  children: Element[];
}

interface Value {
  type: Primitive;
  text: string;
}
```

A `Node` represents a `Struct`-typed or `Flag`-typed element. `Node.type` is the `Type` assigned by
the type assignment algorithm (§20.2). `Node.children` is the ordered list of child elements; for a
`Flag`-typed node, `children` is always empty.

A `Value` represents a `Primitive`-typed element. It is a leaf: it carries the atom text in
`Value.text` and has no children.

The keyword that identified each node is not stored in the semantic model; it can be recovered from
the node's type and the parent's schema structure.

The interpreter directive, pragma, comments, tabulations, and remarks are not part of the semantic
model. There is a one-to-one mapping between presentation-layer atoms and compounds on the one
hand, and elements on the other: every atom and every compound maps to exactly one element.

### 18.3 Mapping Procedure

The mapping from presentation model to semantic model proceeds as follows. The type assignment
algorithm (§20.2) ascribes a type to every atom and compound. Given these assignments, the semantic
tree is constructed by:

1. **Document root.** Create a root `Node` with `type` equal to `Schema.document` and `children`
   constructed from steps 2–5.

2. **Compound children.** For each compound child C of the current node (iterating across all blocks
   in order, skipping comments and tabulations):
   - If the type assigned to C is `Struct` or `Flag`, create a `Node` with that type and `children`
     constructed by recursing into C's children (empty for `Flag`).
   - If the type assigned to C is `Primitive`, create a `Value` with that type and `text` equal to
     the compound's single inline atom text.

3. **Atoms.** For each atom A assigned to the current node (in order):
   - If the assigned type is `Primitive`, create a `Value` with that type and `text` equal to A's
     text.
   - If the assigned type is `Flag`, create a `Node` with that type and an empty `children` list.

4. **Ordering.** Atom-derived nodes and compound-derived nodes for the same member are interleaved
   in the order they were assigned. Atom-derived nodes for a member precede compound-derived nodes
   for the same member (atoms appear on the parent line, before any child lines).

5. **Defaults.** For each required `Product` member with a `Primitive` type and a non-null `default`
   that was not filled by any atom or compound child: create a `Value` with that `Primitive` type
   and `text` equal to `Primitive.default`. This node is placed at the position in the child list
   corresponding to the member's position in member order.

The resulting tree is fully determined by the presentation model and the schema. No ambiguity
remains: the type, value, and child order of every element are fixed by the type assignment
algorithm.

The schema language and type assignment algorithm are defined in §20.

## 19. Schema-Governed Structure and Error Diagnosis

In addition to parsing errors, a CODL document may be structurally invalid with respect to a schema.

When a schema is available, it is applied during parsing rather than as a separate post-processing
stage. This is necessary because the schema informs certain error recovery decisions — in
particular, indentation recovery (§19.5) uses keyword validity at candidate indent levels to resolve
ambiguous lines. The result is a presentation model and semantic model constructed together in a
single pass.

### 19.1 Atom and Compound Interchangeability

Every presentation-layer atom and every presentation-layer compound corresponds to an element,
and every element has a type. The distinction between atom and compound is therefore
presentational rather than semantic.

A child whose type can be uniquely inferred from schema position may be written either as an atom in
atom position or as a compound with an explicit keyword. A child whose type cannot be uniquely
inferred must be written as a compound with an explicit keyword.

Conversely, when a child is written as a compound with an explicit keyword, an implementation may
determine that the same child could have been written positionally as an atom, provided the schema
would have assigned the same type deterministically.

### 19.2 Positional Assignment

For a given parent type, the schema defines an ordered sequence of child specifications.

The order in which child types are specified in the schema determines the order in which inline
atoms are assigned types.

Inline atoms may be assigned types from that ordered sequence so long as the applicable child
specifications are non-repeatable.

If an atom position is assigned to a repeatable child type, then all subsequent inline atoms on that
same compound line must be assigned to that same repeatable child type.

For a `repeatable` member, occurrences may be split across both of the following:

- inline atoms on the parent compound line, and
- subsequent compound children of the parent with the same keyword

These two assignment mechanisms may be combined freely. The E309 contiguity rule (§19.3) already
prohibits differently-typed compound children from being interleaved between such occurrences.
Remark lines do not affect this rule.

### 19.3 Error Taxonomy

Errors are identified by a code of the form **P-NNN** (parsing), **S-NNN** (schema), or **V-NNN**
(validation). Parsing errors indicate violations of the presentation-model syntax. Schema errors
indicate a malformed schema. Validation errors indicate that a document does not conform to its
schema.

Each error is referenced by code at the point in the specification where its trigger condition is
defined.

**Diagnostic spans.** Every diagnostic MUST identify the relevant region of the document as a
half-open span `[start, end)` of zero-based code-point offsets from the start of the document. A
zero-width span (where `start = end`) denotes a point location. The span for each error code is
specified in the tables below.

#### Parsing Errors (E1xx)

| Code | Section  | Description                                                                                        | Span                                                                                                        |
| ---- | -------- | -------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------- |
| E101 | §4       | BOM present at start of document                                                                   | The BOM bytes (`[0, 3)` for a UTF-8 BOM)                                                                    |
| E102 | §8       | Pragma is not the first non-blank line after any interpreter directive                             | The `pragma` keyword on the misplaced line                                                                  |
| E103 | §8       | Pragma line extends beyond the first 4096 bytes                                                    | The entire pragma line                                                                                      |
| E104 | §8       | Pragma version parameter does not have the form `x.y` with non-negative integers                   | The version atom                                                                                            |
| E105 | §8       | Pragma sigil present without a schema identifier                                                   | The sigil atom                                                                                              |
| E106 | §8       | Pragma sigil is a space, newline, letter, or digit                                                 | The sigil atom                                                                                              |
| E107 | §9       | Document contains no non-blank lines                                                               | Zero-width span at the start of the document (`[0, 0)`)                                                     |
| E108 | §9       | Non-blank line begins with fewer than the margin number of spaces                                  | The leading spaces of the line (zero-width at line start if no spaces)                                      |
| E109 | §9       | Relative indentation after the margin is odd                                                       | The leading spaces of the line; extended through subsequent lines if margin adjustment persists (see §19.5) |
| E110 | §9, §17  | Trailing spaces on a non-blank ordinary line or tabulated row                                      | The trailing space characters                                                                               |
| E111 | §11.1    | Comment line not immediately preceded by a blank line                                              | Zero-width span at the start of the comment line                                                            |
| E112 | §14      | Line indent is less than the preceding non-blank line's indent and no ancestor has the same indent | The leading spaces of the line                                                                              |
| E113 | §14      | Line indent exceeds the preceding non-blank line's indent by more than one                         | The leading spaces of the line                                                                              |
| E114 | §14, §17 | Line would become a child of a comment, tabulation, or tabulated row                               | Zero-width span at the start of the line                                                                    |
| E115 | §15      | Source atom introduced when the preceding compound already has a source or literal atom            | The first line of the duplicate source atom                                                                 |
| E116 | §16      | Literal atom introduced when the preceding compound already has a source or literal atom           | The opening delimiter line of the duplicate literal atom                                                    |
| E117 | §16      | Literal atom reaches end of file before its closing delimiter line                                 | The opening delimiter line                                                                                  |
| E118 | §17      | Tabulated row has an indent different from the tabulation line                                     | The leading spaces of the row                                                                               |
| E119 | §17      | Hard space on a tabulated row does not end at a column start boundary                              | The misaligned hard-space run                                                                               |
| E120 | §17      | Consecutive spaces appear within a keyword, pre-column atom, or column value on a tabulated row    | The consecutive space characters within the value                                                           |
| E121 | §17      | Column value exceeds the maximum width for that column                                             | The overflowing column value                                                                                |
| E122 | §11.2    | Malformed tabulation line heading                                                                  | The malformed heading region (from the marker to the next marker or end of line)                            |
| E123 | §4       | `CR` not immediately followed by `LF`, or line-ending mode inconsistency                           | The `CR` character (or `CR LF` pair that violates the established mode)                                     |

#### Schema Errors (E2xx)

| Code | Section | Description                                                                                                               | Span                                           |
| ---- | ------- | ------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------- |
| E201 | §20.1   | `Schema.document` is not a `Struct`                                                                                       | The `document` field in the schema             |
| E202 | §20.1   | Duplicate keyword within a `Struct` (across `Product` keywords and `Sum` variant keywords)                                | The second occurrence of the duplicate keyword |
| E203 | §20.1   | `Sum` member has an empty `variants` list                                                                                 | The `Sum` member definition                    |
| E204 | §20.1   | `Sum` member has a variant whose type is not `Flag`                                                                       | The non-`Flag` variant                         |
| E205 | §20.2   | Root struct has a `required` atom-assignable member (unreachable: the document root has no atoms)                         | The `required` member definition               |
| E206 | §20.1   | `Primitive` has a non-null `default` but appears in a non-`required` member                                               | The `default` field of the `Primitive`         |
| E207 | §20.3   | Two or more `Layer`s within a `Schema` share the same `id`                                                                | The second `Layer` with the duplicate `id`     |
| E208 | §20.3   | A `Layer` `Sum` member has a variant keyword that overlaps with an existing keyword in the base `Struct`                  | The overlapping variant keyword in the layer   |
| E209 | §20.3   | A `Layer` `Product` member matches an existing keyword but the base or layer member is not a `Product` with `Struct` type | The layer member definition                    |
| E210 | §20     | `Schema.sigil` is non-null and is a space, newline, letter, or digit                                                      | The `sigil` field value                        |

#### Validation Errors (E3xx)

| Code | Section | Description                                                                                           | Span                                                                                                     |
| ---- | ------- | ----------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------- |
| E301 | §20.2   | Compound's type is not a `Struct`                                                                     | The compound's keyword                                                                                   |
| E302 | §20.2   | More atoms on a compound than there are assignable member positions                                   | The first excess atom                                                                                    |
| E303 | §20.2   | Atom appears at a member position that is not atom-assignable                                         | The atom                                                                                                 |
| E304 | §20.2   | Atom text matches no variant keyword of a `Sum` member                                                | The atom                                                                                                 |
| E305 | §20.2   | Atom text does not match a `Product` member's `Flag` keyword                                          | The atom                                                                                                 |
| E306 | §20.2   | Compound keyword is not recognized for its parent type                                                | The compound's keyword                                                                                   |
| E307 | §20.2   | Required member absent, and member is not a `Product` with a `Primitive` type with non-null `default` | Zero-width span at the end of the parent compound's last child (or at the parent keyword if no children) |
| E308 | §20.2   | Non-repeatable member is filled more than once                                                        | The keyword of the second occurrence                                                                     |
| E309 | §20.2   | Compound children of the same member are not contiguous                                               | The keyword of the non-contiguous child (the second group's first child)                                 |
| E310 | §21     | Helper method returned an invalid response for a `Primitive` atom value                               | As reported by the helper method's diagnostic spans, translated to document offsets                      |
| E311 | §20.2   | `Flag`-typed compound has atoms or compound children                                                  | The first atom or child of the `Flag` compound                                                           |

### 19.4 Error Diagnosis

Error diagnosis in CODL has at least two layers:

- **parsing diagnosis**, which reports violations of the presentation syntax defined by this
  specification
- **schema diagnosis**, which reports violations that arise when the presentation model is checked
  against a schema and translated into the semantic model

These two layers SHOULD be distinguished in diagnostics.

A conforming implementation SHOULD report multiple independent errors when validating a document,
rather than halting at the first error encountered. Each error has a defined recovery strategy
(§19.5) that allows parsing to continue and subsequent errors to be reported.

Every diagnostic MUST include the error code and the span defined for that error in §19.3. The span
is expressed as a half-open range `[start, end)` of zero-based code-point offsets from the start of
the document.

### 19.5 Error Recovery

For every error condition, a conforming implementation MUST apply the recovery strategy defined here
before continuing. No error SHALL prevent subsequent errors from being reported.

#### Parsing Error Recovery

| Code | Recovery strategy                                                                                                                                                                                                                                                                                                                   |
| ---- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| E101 | Ignore the BOM and continue parsing from the next byte.                                                                                                                                                                                                                                                                             |
| E102 | Restart parsing the entire document using the version, schema identifier, and sigil extracted from the misplaced pragma.                                                                                                                                                                                                            |
| E103 | Allow the pragma line to exceed the 4096-byte limit and continue parsing its content normally.                                                                                                                                                                                                                                      |
| E104 | If the version parameter cannot be parsed as `x.y` at all, ignore it and parse with the latest known version. If it has the correct format but names an unknown version, use the most recent minor version of the same major version if one is known; if the major version itself is unknown, use the latest known version overall. |
| E105 | Treat the sigil as valid even though no schema identifier is present.                                                                                                                                                                                                                                                               |
| E106 | Ignore the invalid sigil and use the default sigil (`#`) instead.                                                                                                                                                                                                                                                                   |
| E107 | Terminal. No recovery is possible or necessary; parsing cannot continue on an empty document.                                                                                                                                                                                                                                       |
| E108 | If the line has exactly one fewer leading space than the current margin, insert a synthetic leading space and parse the line at the current indentation level normally. If the line has two or more fewer leading spaces than the current margin, reset the margin to the line's actual indentation level from that point forward.  |
| E109 | Parse the line's keyword; check which of the two candidate indent levels (±1 space) makes the keyword valid according to the schema; adjust the margin accordingly. See indentation recovery algorithm below.                                                                                                                       |
| E110 | Ignore trailing spaces and parse the remainder of the line normally.                                                                                                                                                                                                                                                                |
| E111 | Ignore the absence of a preceding blank line and treat the comment as normally attached to the following node.                                                                                                                                                                                                                      |
| E112 | Same indentation recovery as E109: use the schema to determine which candidate indent level produces a valid keyword placement, and adjust the margin accordingly.                                                                                                                                                                  |
| E113 | Same indentation recovery as E109: use the schema to determine which candidate indent level produces a valid keyword placement, and adjust the margin accordingly.                                                                                                                                                                  |
| E114 | Same indentation recovery as E109: the line cannot be a child of its apparent parent (a comment, tabulation, or row), so treat it as if indented one level less and use the schema to validate the adjusted placement.                                                                                                              |
| E115 | Ignore the duplicate source atom; use the first one encountered.                                                                                                                                                                                                                                                                    |
| E116 | Ignore the duplicate literal atom; use the first one encountered.                                                                                                                                                                                                                                                                   |
| E117 | Treat the unclosed literal atom's payload as everything from the opening delimiter line to the end of file (excluding the final newline, if any).                                                                                                                                                                                   |
| E118 | Interpret the tabulated row according to its actual hard-space positions regardless of alignment with column markers. Suppress any further alignment errors (E119, E120, E121) on the same row.                                                                                                                                     |
| E119 | Same as E118.                                                                                                                                                                                                                                                                                                                       |
| E120 | Same as E118.                                                                                                                                                                                                                                                                                                                       |
| E121 | Same as E118.                                                                                                                                                                                                                                                                                                                       |
| E122 | Report the error and continue parsing, but disable column-alignment checking for the remainder of the current tabulated block.                                                                                                                                                                                                      |
| E123 | Treat any malformed sequence of consecutive `CR` and `LF` characters as a single line break if it contains at most one `CR` and at most one `LF`; treat it as two line breaks if either `CR` or `LF` appears more than once in the sequence.                                                                                        |

#### Validation Error Recovery

All validation errors (E301 through E311) are self-contained: they do not have cascading effects on
the remainder of the type assignment or validation process. An implementation MUST record the error
and continue processing remaining nodes as if the erroneous node were absent or were assigned the
most plausible available type. Specific recovery notes:

- **E311** (`Flag` compound with atoms or children): ignore the atoms and children of the `Flag`
  compound; treat it as a bare keyword with no content.

#### Indentation Recovery (E109, E112, E113)

When a line's relative indentation after the margin is odd (E109), the line sits between two valid
indentation positions: one space deeper and one space shallower. Recovery uses the schema to resolve
the ambiguity.

**Algorithm.** When an odd-indented line is encountered:

1. Parse the line to determine its keyword.
2. Let the two candidate indent levels be _deeper_ (adding one space to align to the next even
   level) and _shallower_ (removing one space to align to the previous even level).
3. If adjusting the margin by +1 space would make the keyword valid at the resulting indent level
   (according to the schema's expected types at that position), adopt that adjustment. If adjusting
   by −1 space would make the keyword valid, adopt that adjustment instead. If both or neither
   direction produces a valid keyword, the implementation SHOULD prefer the shallower
   interpretation.
4. Record an E109 error whose span begins at this line. Adjust the effective margin accordingly for
   all subsequent lines.

**Subsequent odd-indentation lines.** If a later line is also odd-indented:

- If the margin adjustment required is in the **opposite direction** to the current adjustment
  (i.e., the adjustment would restore the original even margin), it is not a new error. Instead, the
  span of the original E109 error is extended to cover all lines up to the point where the original
  margin is restored.
- If the margin adjustment is in the **same direction** as the current adjustment (i.e., the margin
  shifts further away from the original), a new E109 error is recorded at that point.

**E112 and E113.** These errors (unmatched dedent and over-indentation) are resolved by the same
mechanism: the parser uses the schema to determine which of the candidate indent levels produces a
valid keyword placement. The margin adjustment and span-extension rules apply identically.

## 20. Schema Language

A schema is expressed using the following types:

```typescript
interface Schema {
  id: string;
  document: Type;
  layers: Layer[];
  sigil: Sigil | null;
}

interface Layer {
  id: string;
  root: Struct;
}

type Type = Struct | Primitive | Flag;

interface Struct {
  members: Member[];
}

interface Primitive {
  validator: string;
  default: string | null;
}

interface Flag {}

type Member = Product | Sum;

interface Product {
  required: boolean;
  repeatable: boolean;
  keyword: string;
  type: Type;
}

interface Sum {
  required: boolean;
  repeatable: boolean;
  variants: Variant[];
}

interface Variant {
  keyword: string;
  type: Type;
}
```

`Schema.id` is an ASCII string identifier for the schema. It MUST consist only of printable ASCII
characters other than whitespace.

`Schema.document` is the type of the document root. It MUST be a `Struct`.

`Schema.layers` is an ordered list of `Layer` values defining optional schema extensions. The empty
list is the normal case for a schema with no layers. Layer composition is defined in §20.3.

`Schema.sigil` is the default sigil for documents that use this schema, or `null` if the schema does
not declare one. When non-null, it MUST satisfy the same character constraints as a pragma sigil
(§8): it MUST NOT be a space, newline, letter, or digit (**E210**). When a document's pragma omits a
sigil but provides a schema identifier that resolves to a schema with a non-null `sigil`, the
schema's sigil is used as if it had been specified in the pragma (§8.3).

CODL schemas are themselves representable as CODL documents. The CODL schema that describes the CODL
schema language is therefore self-describing; the schema for schemas is identified by the schema ID
`codl-schema`. The serialization of a schema as a CODL document is governed by that schema.

A `Struct` has an ordered list of `Member`s. Each member describes one logical child slot of the
struct and is either a `Product` or a `Sum`. Both carry the common properties `required` and
`repeatable`:

- `required`: if `true`, the slot MUST be present at least once in a conforming document
- `repeatable`: if `true`, the slot MAY appear more than once; if `false`, it MUST appear at most
  once

A `Product` member has a single `keyword` and a single `type`. It represents a child whose keyword
and type are fixed.

A `Sum` member has a non-empty list of `Variant`s. Any variant's keyword may be used to fill that
slot; the chosen keyword determines the type of the child node placed in that slot.

`Variant.keyword` is the keyword by which a child compound of that variant is written in CODL when
explicit. `Variant.type` may be any `Type`.

A `Primitive` type represents a leaf value constrained by a validator. `Primitive.validator` names
the helper method to be invoked to validate the atom text (§21). `Primitive.default` is either
`null` (no default) or a string giving the value to be used when the member is absent. A non-null
default MAY only be specified if the `Primitive` appears in a `required: true` member; specifying a
non-null default on a non-required member is a schema error (**E206**). When a required `Product`
member whose type is a `Primitive` with a non-null default is absent from the document, the default
value is used as the semantic value and no E307 error is raised.

A `Flag` type carries no value of its own. Its identity is entirely determined by its keyword
(`Product.keyword` or `Variant.keyword`): in compound position, a `Flag`-typed node is written as
the keyword alone, with no inline atoms; in atom position, the atom text is matched against the
keyword. A `Flag`-typed member SHOULD NOT be `required`, since a required `Flag` member would be
unconditional boilerplate.

**Member ordering recommendation.** The order of members in a `Struct` determines which children can
be serialized as inline atoms (see the `construct` operation in §22.2). To maximize the use of
inline atoms, schema authors SHOULD order members as follows:

1. Required `Product` members with `Primitive` type (most likely to always be present).
2. Non-required `Product` members with `Primitive` type, prioritizing those most likely to be
   specified rather than absent.
3. Either a `Sum` member (all `Flag` variants) or a single `repeatable` `Product` member with
   `Primitive` type — but not both, since only one of these can appear in the trailing atom
   position.
4. All remaining members (`Struct`-typed products, sums not covered above, and any further members),
   which will always be serialized as compound children.

This ordering is a recommendation, not a requirement. Any member order is valid.

**Member order.** The **member order** of a `Struct` is the sequence of its members in their
declaration order within `Struct.members`. Where a specification rule refers to members "in member
order", it means iterating `members[0]`, `members[1]`, …, `members[n−1]`.

**Keyword order.** The **keyword order** of a `Struct` is a flat sequence of (keyword, type) pairs
obtained by expanding each member in member order: a `Product` member contributes a single entry
(its keyword and type); a `Sum` member contributes one entry per variant, in the declaration order
of `Sum.variants`. Keywords are numbered from 0 in this sequence; the position of a keyword in
keyword order is its **keyword index**.

**Identifier naming convention.** Programmatic identifiers defined by this specification — including
helper method names in `Primitive.validator` and the edit operation identifiers in §22.2 — use
**kebab-case**: a sequence of lowercase ASCII words separated by hyphens (e.g. `update-value`,
`switch-variant`). Schemas SHOULD use kebab-case for validator names.

Every kebab-case identifier corresponds to a unique sequence of lowercase words. Implementations
SHOULD represent these identifiers idiomatically in their host language by applying the equivalent
convention:

- **kebab-case** (`update-value`) — the canonical form used in schemas and in this specification
- **snake_case** (`update_value`) — e.g. Rust, Python
- **camelCase** (`updateValue`) — e.g. Java, TypeScript, JavaScript
- **PascalCase** (`UpdateValue`) — e.g. C#, Go exported names

The mapping between these conventions is an isomorphism over sequences of lowercase words:
implementors SHOULD expect identifiers to appear in the idiomatic style of the host language and
MUST map them back to kebab-case when comparing against schema-defined names.

### 20.1 Schema Validity Constraints

A schema is invalid if any of the following holds:

- `Schema.document` is not a `Struct` (**E201**)
- within a single `Struct`, the same keyword appears more than once across all members (considering
  `Product.keyword` and every `Variant.keyword` within each `Sum`) (**E202**)
- a `Sum` member has an empty `variants` list (**E203**)
- a `Sum` member has a variant whose type is not `Flag` (**E204**)
- a `Primitive` has a non-null `default` and appears in a `Member` with `required: false` (**E206**)
- `Schema.sigil` is non-null and is a space, newline, letter, or digit (**E210**)

### 20.2 Type Assignment Algorithm

Type assignment translates the presentation model into the semantic model by ascribing a type to
every atom and compound node in the tree. It proceeds as a recursive descent over the tree, guided
by the schema.

**Atom-assignable members.** A `Product` member M is _atom-assignable_ if M.type is `Primitive` or
`Flag`. A `Sum` member is always atom-assignable (schema validity E204 ensures all its variants are
`Flag`). A member that is not atom-assignable may only be satisfied by compound children (written
with an explicit keyword), not by inline atoms.

**Document root.** The document root is a virtual compound node with type `Schema.document`. It has
no atoms; any `required` atom-assignable members of the root struct cannot be satisfied (**E205**).

**Type assignment for a compound node N with type T:**

1. T MUST be a `Struct`; if it is not, the document is invalid (**E301**).

2. Construct the keyword map K by iterating T in keyword order: for each entry (keyword, type) at
   member index i, map keyword → (i, type). (Schema validity ensures no duplicate keywords within
   the same struct.)

3. **Atom phase.** Let `pos` = 0. For each atom A in N.atoms, in order:

   a. Advance `pos` while the following skip condition holds: `pos` < len(T.members), the member M
   at `pos` is not `required`, M is a `Sum` or M is a `Product` with `Flag` type, and the text of A
   does not match any keyword of M (i.e., M.keyword for a `Product`, or any variant's keyword for a
   `Sum`). Each advanced-past member is recorded as absent.

   b. If `pos` ≥ len(T.members), the document is invalid (**E302**: more atoms than assignable
   member positions).

   c. Let M = T.members[pos]. M MUST be atom-assignable; if it is not, the document is invalid
   (**E303**: atom in non-atom-assignable member position).

   d. Assign A to M:
   - If M is a `Sum`, the matched variant is the one whose keyword equals A's text; if no variant's
     keyword matches, the document is invalid (**E304**).
   - If M is a `Product` with `Flag` type, A's text MUST equal M.keyword; if it does not, the
     document is invalid (**E305**).
   - If M is a `Product` with `Primitive` type, the type of A is M.type regardless of A's text
     (validation against the named helper method is a separate step; see §21).

   e. If M is not `repeatable`, increment `pos`. If M is `repeatable`, leave `pos` unchanged; all
   subsequent atoms are also assigned to M.

4. **Compound child phase.** Let `current_member` = −1 and `seen_members` = {} (empty set). For each
   compound child C in N.children (iterating across all blocks in order):

   a. Look up C.keyword in K. If not found, the document is invalid (**E306**: unrecognized keyword
   for this parent type).

   b. Let (i, childType) = K[C.keyword]. The type of C is childType.

   c. If i ≠ `current_member`: if i is in `seen_members`, the document is invalid (**E309**: member
   children not contiguous); otherwise add `current_member` to `seen_members` (if ≥ 0) and set
   `current_member` = i.

   d. Record that member at index i has been filled by C.

   e. Recursively apply type assignment to C with type childType.

5. **Constraint check.** For each member M in T.members:

   a. Let fill_count = (number of atoms assigned to M) + (number of compound children assigned to
   M).

   b. If M.`required` and fill_count = 0: if M is a `Product` whose type is a `Primitive` with a
   non-null `default`, the default value is used as the semantic value and no error is raised;
   otherwise, the document is invalid (**E307**: required member absent and no default available).

   c. If not M.`repeatable` and fill_count > 1, the document is invalid (**E308**: non-repeatable
   member filled more than once).

### 20.3 Schema Layering

A `Schema` may include one or more `Layer` values in its `layers` list. Each layer describes an
incremental extension to the schema's root struct. Layers are append-only: they may add new members
to a struct but may not delete existing members or alter the `required` or `repeatable` properties
of existing members.

**Composed schema identity.** A composed schema is identified by the base schema's `id` together
with the ordered sequence of layer `id`s applied to it. Two schemas with the same base `id` but
different layer sequences are distinct schemas.

**Merge algorithm.** The function `Merge(base: Struct, layer: Struct): Struct` produces a new struct
that incorporates the layer's members into the base:

1. Begin with a copy of `base.members` in member order.

2. Construct the keyword map K for the base struct by iterating it in keyword order: for each entry
   (keyword, type) at member index i, map keyword → (i, members[i]).

3. For each member L in `layer.members` in member order:

   a. **Product members.** If L is a `Product` (keyword W, type T):
   - Look up W in K.
   - **Found:** Let (i, M) = K[W]. M MUST be a `Product` and both M.type and T MUST be `Struct`; if
     either is not, the layer is invalid (**E209**). Replace M.type in the merged member list at
     index i with `Merge(M.type, T)`.
   - **Not found:** Append L as a new member at the end of the member list. Add W → (new index, L)
     to K.

   b. **Sum members.** If L is a `Sum`:
   - Every variant keyword in L.variants MUST be absent from K; if any variant's keyword already
     exists in K, the layer is invalid (**E208**).
   - Append L as a new member at the end of the member list. For each variant V in L.variants, add
     V.keyword → (new index, L) to K.

4. Return the resulting member list as the merged struct.

**Layer validity constraints.** A schema is invalid if any of the following holds:

- Two or more layers within the same schema share the same `id` (**E207**)
- A layer `Sum` member has any variant keyword that already appears in the keyword map of the
  (progressively merged) base struct (**E208**)
- A layer `Product` member matches an existing keyword, but the base member is not a `Product`, or
  the base type, the layer type, or both are not `Struct` (**E209**)

**Composing layers.** To apply a sequence of layers `[L₁, L₂, …, Lₙ]` to a base schema with root
struct R, apply `Merge` iteratively: start with R₀ = R, then Rₖ = `Merge(Rₖ₋₁, Lₖ.root)` for k = 1 …
n. The final Rₙ is the root struct of the composed schema.

### 20.4 BCODL and Schema Hashing

**BCODL** is the binary encoding of the semantic model of a CODL document. Every well-typed CODL
document has exactly one BCODL encoding; the mapping is fully deterministic. A schema is itself a
CODL document and therefore has a BCODL encoding.

**Value hash.** The **value hash** of a CODL document is the SHA-256 digest of its BCODL
representation excluding the magic number and schema signature — that is, the hash is computed over
only the document root encoding (§20.4.4). This is the general method for hashing any semantic CODL
value.

When used in schema identifiers (§8.1), the hash is represented as a BASE64-URL-encoded (no padding)
string of 44 characters.

#### 20.4.1 Integer Encoding

All counts and byte-lengths in BCODL are non-negative integers encoded in a variable-length format.
To encode an integer N:

1. Set B = N & 0x7F (the seven least-significant bits of N).
2. Set N = N >> 7.
3. If N > 0, set bit 7 of B (i.e. B = B | 0x80) and write the byte B; then repeat from step 1.
4. If N = 0, write B as the final byte (bit 7 is clear).

The result is one or more bytes. Every byte except the last has bit 7 set (a **continuation byte**).
The last byte has bit 7 clear. The seven low-order bits of each byte, concatenated from
least-significant (first byte) to most-significant (last byte), reconstruct the original integer.

Decoding: read bytes in sequence; for each byte, take bits 0–6 and OR them into the accumulator at
the current bit offset; advance the bit offset by 7. If bit 7 of the byte is set, read the next
byte; otherwise the integer is complete.

| Value | Encoded bytes (hex) |
| ----: | ------------------- |
|     0 | `00`                |
|     1 | `01`                |
|   127 | `7F`                |
|   128 | `80 01`             |
|   255 | `FF 01`             |
| 16383 | `FF 7F`             |
| 16384 | `80 80 01`          |

#### 20.4.2 Keyword Index

The keyword index used in BCODL node encoding is the position of a keyword in keyword order (§20).
Because the schema determines the type of every node from its keyword index and its parent's type,
BCODL encodes no type tags.

#### 20.4.3 File Layout

A BCODL file consists of the following fields in order:

1. **Magic number**: the 2 bytes `C0 D1`
2. **Schema signature**: the byte length of the signature (integer), followed by the signature
   bytes. The schema signature identifies the composed schema (base plus layers) used to type the
   document. Its construction is defined in §20.4.5.
3. **Document root**: encoded as described in §20.4.4 (root form).

#### 20.4.4 Node Encoding

**Document root.** The root is a virtual struct with no parent keyword. It is encoded as:

1. The number of top-level child nodes (integer).
2. Each top-level child node in order, using the struct, primitive, or flag encoding below.

**Struct node** (schema type is `Struct`):

1. The keyword index of this node (integer).
2. The number of child nodes (integer).
3. Each child node in order, recursively.

**Primitive node** (schema type is `Primitive`):

1. The keyword index of this node (integer).
2. The byte length of the UTF-8 encoding of the value string (integer).
3. The UTF-8-encoded bytes of the value string.

**Flag node** (schema type is `Flag`):

1. The keyword index of this node (integer).

**Default values.** BCODL encodes the semantic model, in which a required `Primitive` member with a
non-null default is semantically present even when it was absent from the source document.
Therefore, when encoding a document to BCODL, a missing required primitive whose default is used
MUST be encoded as a primitive node with the default value string. This ensures that the BCODL
encoding is identical regardless of whether the member was explicitly written or filled by its
default.

There are no pad bytes, alignment constraints, or inter-node delimiters. The schema provides all
type information needed to decode the stream unambiguously.

#### 20.4.5 Schema Signature

A schema signature identifies a composed schema as an ordered sequence of components: a base schema
followed by zero or more layers. Each component is identified by its value hash (§20.4).

A schema file defines a base schema and zero or more layers. Each component's hash is computed
independently by encoding that component as a BCODL document root and taking the SHA-256 digest of
the root encoding (excluding magic number and signature).

**Encoding.** Given an ordered sequence of n component hashes h₀, h₁, …, h\_{n−1} (each 256 bits),
the signature is computed as follows:

1. Let S = 0 (a zero-valued integer of unbounded width).
2. For each hash hᵢ, in order from i = 0 to i = n−1: a. Set S = (S << 8) XOR hᵢ.
3. The result S has a width of 256 + (n−1)×8 bits, or equivalently 31 + n bytes.

Emit the signature as `31 + n` bytes, most-significant byte first.

**Correctness property.** Because each shift is only 8 bits wide but each hash is 256 bits wide, the
lowest 8 bits of S are determined solely by the last hash h*{n−1}. After XORing h*{n−1} out of S and
shifting right by 8 bits, the lowest 8 bits are determined solely by h\_{n−2}. This property holds
at every step, enabling unambiguous decoding.

**Decoding.** Given a signature S of known byte length L, and a set of candidate hashes H (the value
hashes of all components defined in the schema file):

1. Compute n = L − 31. This is the number of components.
2. Let the output sequence be empty.
3. Repeat n times: a. Let b = S & 0xFF (the lowest byte of S). b. Find all hashes in H whose lowest
   byte equals b. c. For each candidate hash h: compute S′ = (S XOR h) >> 8. d. Recurse with S = S′
   and the candidate h appended to the front of the output sequence.
4. When n steps have been completed, S MUST be zero. If S ≠ 0, the candidate path is invalid;
   backtrack and try the next candidate.
5. Exactly one valid sequence MUST exist. If no valid sequence is found, or if more than one is
   found, the signature is malformed.

The decoded sequence gives the component hashes in order: h₀ (base schema), h₁ (first layer), …,
h\_{n−1} (last layer). A BCODL decoder uses this sequence to locate and compose the schema before
decoding the document root.

Schema compatibility is defined in §8.2 in terms of subsequence relationships between decoded
signature hash sequences.

## 21. Validation

Type assignment (§20.2) ascribes a `Type` to every node. For `Struct` and `Flag` types, the
structure of the document is sufficient to determine validity. For `Primitive` types, the atom text
must additionally be checked against a named **validator**.

### 21.1 Validators

A validator is identified by the string in `Primitive.validator`. This string names an external
**helper method** that checks whether a given string value conforms to the primitive's constraints.

Validation of primitive values is, in general, too complex to express within a schema: it may
require external data, complex algorithms, or application-specific logic. CODL therefore delegates
primitive validation to helper methods provided outside the schema.

### 21.2 Request and Response

The request and response of a helper method invocation are defined by the following types:

```typescript
interface ValidationRequest {
  method: string;
  value: string;
}

type ValidationResponse = Valid | Invalid;

interface Valid {}

interface Invalid {
  diagnostics: Diagnostic[];
}

interface Diagnostic {
  message: string;
  start: number;
  end: number;
}
```

`ValidationRequest.method` is the value of `Primitive.validator`. `ValidationRequest.value` is the
verbatim text of the primitive atom (whether `Inline`, `Source`, or `Literal` in the presentation
model — the atom form is not semantically significant).

A helper method MUST return either `Valid` or `Invalid`. An `Invalid` response includes a non-empty
list of `Diagnostic` entries. Each entry has a human-readable `message` and a half-open span
`[start, end)` of zero-based code-point indices into the input string, identifying the portion of
the input to which the message applies.

In many cases a single diagnostic entry spanning the entire input string is sufficient. However, a
helper method MAY return multiple entries highlighting different errors at different positions
within the input. Spans MAY overlap.

When reporting an E310 error, the implementation MUST translate each helper method span from
input-string-relative offsets to document-level code-point offsets by adding the offset of the
atom's first content character within the document.

### 21.3 Integration

The `Primitive.validator` field in the schema specifies which helper method provides validation for
that primitive type. A conforming implementation MUST invoke the named helper method for every
`Primitive` atom in the document during validation, unless the implementation explicitly opts out of
primitive validation (for example, during a parse-only pass that does not require full semantic
checking).

If the helper method returns an invalid response, the document is invalid (**E310**). The
implementation SHOULD report each diagnostic entry to the user, associating the span with the
corresponding source location in the original document.

### 21.4 Helper Method Binding

A CODL parser that wishes to enable primitive validation MUST be provided with a **callback
function** that conforms to the helper method interface: given a `ValidationRequest`, it returns a
`ValidationResponse`. The parser invokes this callback for each `Primitive` atom encountered during
validation. How the callback is supplied is determined by the host language or environment (e.g. as
a function parameter, a trait implementation, or an interface injection).

This specification does not prescribe a wire protocol, service discovery mechanism, or serialization
format for helper method invocation. In particular:

- A parser embedded in an application MAY implement the callback directly in the host language.
- An IDE, text editor, or LSP server MAY delegate helper method calls to an external service (e.g.
  via REST, RPC, or a subprocess), but the mechanism by which the editor discovers and configures
  such a service is outside the scope of this specification. From the parser's perspective, the
  editor simply provides a callback that handles the delegation internally.

If no callback is provided, the parser MUST skip primitive validation entirely (no E310 errors are
raised). All other parsing and validation proceeds normally.

## 22. Reserialization and Editing

The presentation model can be mutated to reflect changes to the semantic model, preserving
formatting, comments, tabulations, and remarks wherever possible. Mutations are expressed as
operations on the semantic model, which are then reflected in the presentation layer.

There are two categories of editor:

- **Human editors**, who modify source text directly with full flexibility and no constraints on
  what changes may be made
- **Computer editors** (programmatic transformations), which apply structured operations to the
  semantic model and reserialize through the presentation layer

### 22.1 Comment Attachment and Editing

Each `Block` in the presentation model carries zero or more attached comments (§11.1) that precede
its compounds. These comments travel with the block under programmatic transformations.

When a computer editor deletes a compound, the `Block` that contained it is updated. If the deleted
compound was the only compound in its block, and if the block has attached comments, those comments
are also removed (since their meaning was associated with that block).

When a computer editor moves a compound, its containing block's structure is preserved where
possible: if the move results in the block having no remaining compounds, the block (and any
attached comments) moves with the compound to the new location.

When a computer editor inserts a compound constructed from purely semantic information, no comment
is attached to it initially; it is placed into an existing block or a new empty block as
appropriate.

### 22.2 Computer Editor Operations

A computer editor MUST perform only operations drawn from the following set. Each operation
preserves all presentation-layer details that are not directly affected by the operation: remarks,
`trailingBlankLines` counts, `precedingSpaces` on inline atoms, and tabulation marker offsets are
all retained unless the operation explicitly targets them.

**`delete`** — Remove a compound that is not `required`. Any remark attached to the compound is
removed with it. If the compound's block becomes empty (no remaining compounds), the block and its
attached comments are also removed.

**`replace`** — Substitute a compound for another of the same member type at the same position in
the same block. The replacement retains the original compound's remark and its position within the
block. If the member is a `Sum` and the replacement uses a different variant, the keyword in the
presentation layer is updated accordingly. Attached comments on the block are preserved.

**`construct`** — Create a new compound from purely semantic information, with no presentation-layer
context. The constructed compound carries no remark and has no attached comments. No blank lines
appear between its children. No tabulation is added. The canonical presentation form is determined
by iterating the struct's members in member order:

1. Starting from the first member, each non-repeatable `Product` member whose type is `Primitive` is
   serialized as an inline atom, in member order, for as long as consecutive members satisfy this
   condition.
2. If the next member after the initial run of non-repeatable primitives is a `Sum` (all `Flag`
   variants), each present flag is serialized as an inline atom.
3. Otherwise, if the next member is a `repeatable` `Product` whose type is `Primitive`, each
   occurrence is serialized as an inline atom.
4. All remaining children — including any `Product` members whose type is `Struct`, any `Sum`
   members not covered by step 2, and any members beyond the first repeatable primitive — are
   serialized as compound children with explicit keywords.

Each inline atom uses a single preceding space (`precedingSpaces = 1`). Each compound child is
indented by one level (two spaces) relative to its parent.

**`insert`** — Insert a compound into the child structure of a parent at the natural position for
its member: after all existing compounds of the same member, within the same block if one exists for
that member group, or in a new block otherwise.

**`insert-before`** — Insert a compound immediately before a specified existing sibling compound.
The inserted compound is placed in the same block as the sibling if the block does not have a
tabulation, or in a new block immediately before the sibling's block if it does.

**`insert-after`** — Insert a compound immediately after a specified existing sibling compound,
subject to the same block-placement rules as `insert-before`.

**`insert-into-block`** — Append a compound to the `compounds` list of a specified existing block.
This is the natural way to add rows to a tabulated block. The block's tabulation must have
sufficient column capacity for the new compound; if not, `resize-tabulation` must be applied first.

**`attach-remark`** — Add a remark string to a compound. If the compound already has a remark, it is
replaced.

**`remove-remark`** — Remove the remark from a compound.

**`update-value`** — For a compound or atom whose schema type is `Primitive`, update the atom text
to a new string. The new string MUST be valid according to the named helper method (§21). All other
presentation details of the compound are retained.

**`set-flag`** — Add a `Flag`-typed node within a parent, provided the result satisfies the
`repeatable` constraint for that member. The flag is placed as an inline atom if both of the
following hold: (a) the flag's member precedes all compound children in member order (i.e., no
member that currently has compound children appears earlier), and (b) inserting the atom does not
require moving any existing compound children to atom position. If either condition is not met, the
flag is placed as a compound child using the `insert` placement rules.

**`unset-flag`** — Remove a `Flag`-typed node within a parent, provided the result satisfies the
`required` constraint for that member. If the flag is currently an inline atom, the atom is removed
and the `precedingSpaces` of subsequent atoms are preserved. If the flag is currently a compound
child, it is removed using the `delete` rules.

**`reorder-within-group`** — Change the position of a compound among its siblings within the same
member group (i.e., other compounds filling the same schema member). This operation never violates
E309. The reordered compound retains its remark; attached comments on the affected blocks are
preserved.

**`reorder-groups`** — Change the relative order of two distinct member groups within a parent's
child structure, by moving all blocks belonging to one group before or after all blocks belonging to
another. This is valid as long as neither group is interleaved with the other (E309 is satisfied
before and after). Attached comments on all affected blocks are preserved.

**`resize-tabulation`** — Adjust the `markerOffsets` of a block's `Tabulation` to accommodate all
current column values and any values about to be added. New offsets MUST be chosen such that every
existing and planned column value fits within the column widths defined by §11.2, and MUST be a
minimal adjustment (columns are not widened beyond what is required). After resizing, all existing
row content MUST be re-padded with spaces to align to the new column positions. The `headings` list
is updated in parallel with `markerOffsets`: existing headings are preserved in place and re-padded
within their updated column spans; no heading text is added or removed by this operation.

### 22.3 Canonical Document Serialization

A **canonical serialization** of a semantic model produces a single, deterministic CODL text
representation. Canonical serialization follows the same conventions as the `construct` operation
(§22.2) for individual compounds, extended to the entire document:

- The document margin is zero.
- No interpreter directive is included.
- A pragma line is included, specifying the CODL version of the serializer and the schema
  identifier. The sigil is not specified in the pragma (the default `#` is used).
- No comments or remarks are included anywhere in the document.
- No tabulation lines are included; all compounds are serialized as ordinary (non-tabulated) lines.
- No blank lines appear between children at any level.
- The root node has no inline atoms (the document root is a virtual struct with no atom positions),
  so every root-level member is serialized as a compound child.
- At every non-root level, inline atoms are used wherever possible, following the `construct`
  conventions: non-repeatable primitives first, then flags or one repeatable primitive, then
  compound children for everything else.
- Each inline atom uses a single preceding space (`precedingSpaces = 1`).
- Each compound child is indented by one level (two spaces) relative to its parent.
- Line endings use LF mode.

Two documents with identical semantic models, serialized canonically by the same version of the
specification, MUST produce identical text output.

## 23. Invalidity Conditions

A CODL document is invalid if any condition identified by a **E1xx** or **E3xx** error code in this
specification is triggered. A schema is invalid if any **E2xx** condition is triggered. The complete
taxonomy of all error conditions, their trigger sections, and their recovery strategies are given in
§19.3 and §19.5 respectively.

## 24. Deferred Topics

The following topics remain underspecified or unresolved:

- **Complete error taxonomy.** Error codes E101–E123 (parsing), E201–E210 (schema), and E301–E311
  (validation) are believed to be complete. A malformed schema _document_ (as opposed to a
  malformed schema _definition_) is not a separate error category: since schemas are CODL documents
  typed by the `codl-schema` schema, errors in a schema document are ordinary parsing and
  validation errors reported against that schema. No additional error codes are believed to be
  needed, but this has not been exhaustively verified.

- **Empty-string primitives.** A `Primitive` atom whose text is the empty string is syntactically
  valid (an atom with no content after the keyword). What is missing: whether an empty string is a
  valid input to a helper method, whether it is distinguishable from an absent member, and how it
  interacts with `Primitive.default`.

- **Canonical serialization.** Canonical document serialization is defined in §22.3. No further
  specification is believed to be needed, but the interaction between canonical serialization and
  source/literal atom forms has not been fully explored (canonical form currently always uses inline
  atoms).

- **Mutation semantics.** The computer editor operations (§22.2) are defined individually. What is
  missing: the semantics of composing multiple operations (ordering, atomicity, conflict
  resolution), and the precise rules for how `replace` determines what constitutes a valid
  replacement.

- **Examples and reference algorithm.** No worked examples or reference parsing algorithm are
  provided. What is missing: example CODL documents with their presentation models, semantic models,
  and BCODL encodings; a reference implementation or pseudocode for the parsing algorithm; and
  example schema definitions with layering.
