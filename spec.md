# CODL Specification Draft

## 1. Status

This document is a draft specification of CODL.

Where this draft contains `FIXME` notes, the corresponding behavior is not yet fully specified and MUST NOT be considered stable.

## 2. Conformance Language

The key words **MUST**, **MUST NOT**, **REQUIRED**, **SHALL**, **SHALL NOT**, **SHOULD**, **SHOULD NOT**, **RECOMMENDED**, **MAY**, and **OPTIONAL** in this document are to be interpreted as described in RFC 2119 and RFC 8174 when, and only when, they appear in all capitals.

## 3. Overview

CODL is a Unicode, character-based language for ordered, tree-structured data represented as strings, and typed according to a schema.

CODL presents data as an _ordered_ tree, however an application consuming CODL MAY choose to assign meaning to sibling order, or MAY treat it as insignificant. In this respect, CODL is similar to XML.

CODL distinguishes between:

- a **presentation model**, which preserves comments, interpreter directives, pragma metadata, atom presentation form, most whitespace and document structure sufficiently for faithful reserialization, and
- a **semantic model**, which is derived from the presentation model using a schema.

This document specifies CODL source, its parsing into the presentation model, the definiton of schemas and translation between presentation model and semantic model by means of a schema.

## Abstract

Structured data is widely stored and exchanged in formats such as JSON, XML and YAML. These formats may be read and written by humans and machines, with varying convenience for human editors and parser implementors.

For applications which require the exchange of information through a data language across the human/machine boundary, certain features are desirable, particularly on the side of human producers and consumers.

- Details like formatting and plain-language comments should not be left intact after machine modifications
- AI agents, with some characteristics of humans and machines, will increasingly be responsible for editing data files
- Direct machine-to-machine exchange should not be unnecessarily verbose or facilitate ambiguity
- Character escaping, where data language syntax conflicts with content, should be avoided or minimized
- For line-based version control, insignificant changes should not leak onto lines that are semantically unchanged
- Parsing should be strict to avoid ambiguity, but one error should not preclude the detection of others
- Over time, data formats may evolve and support ad-hoc extensions which should remain compatible, but backwards and forwards compatibility should be clear and predictable
- Validation rules for strings may be too complex to represent in a pure data format, but should still be applied
- Strictly-typed and checked values limit the possibility of exchanging invalid data
- Editors and IDEs are widely used and can provide immediate feedback to human editors on formatting errors, type errors and validation errors

CODL addresses all of these features.

## 4. Character Encoding

CODL is defined over Unicode code points.

When serialized as binary data, a CODL document MUST be encoded as UTF-8.

CODL uses `U+000A` LINE FEED (`LF`) as its primary line-ending character. Carriage return (`CR`, `U+000D`) is also permitted under the following rules.

A `CR` appearing anywhere in a CODL document outside a literal atom payload MUST be immediately followed by `LF` (**P-023**).

The **line-ending mode** of a document is determined by the first `LF` character in the document:

- if that `LF` is immediately preceded by `CR`, the mode is **CRLF mode**
- otherwise, the mode is **LF mode**

Once the mode is established, every subsequent `LF` outside a literal atom payload MUST conform to it: in CRLF mode every `LF` MUST be preceded by `CR`; in LF mode `CR` MUST NOT appear before any `LF`. A violation of this rule is also a **P-023** error.

If the document contains no `LF` characters outside literal atom payloads, no mode is established and no P-023 errors can arise.

No Unicode normalization is required or implied. CODL is defined over the exact Unicode code points that appear in the serialized text.

A UTF-8 byte order mark MUST NOT appear in a CODL document (**P-001**).

### 4.1 Recommendations

FIXME: each of these recommendations should be moved to the relevant part of the document.

The following are recommendations rather than validity rules:

- visually misleading code points, such as zero-width characters, SHOULD be avoided
- control-heavy content SHOULD be avoided except where required
- although non-ASCII keywords are permitted, ASCII keywords are generally RECOMMENDED for interoperability and readability
- CODL is not intended primarily as a binary-data format, even though it can represent content containing non-printing code points

## 5. Significant Characters and Terms

The following characters have syntactic significance in CODL:

- `U+000A` LINE FEED (`LF`)
- `U+0020` SPACE
- the configured comment marker (§8.3)

A **line** is a contiguous, potentially empty sequence of non-linefeed characters delimited by linefeed characters or by the start or end of the file.

A **soft space** is exactly one `U+0020` SPACE character.

A **hard space** is two or more consecutive `U+0020` SPACE characters.

A **blank line** is a line containing only `U+0020` SPACE characters, or no characters at all.

Blank lines have no defined indent.

A **word** is a maximal contiguous sequence of non-newline, non-separator characters on a line, where separators are determined by the word-separation rules (§10.3).

An **ordinary line** is any non-blank line that is not a comment line (§11.1), a tabulation line (§11.2), or a line forming part of a source atom (§15) or literal atom (§16) payload.

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
  commentMarker: string | null;
}
```

## 7. Interpreter Directive

If the first two characters of the document are `#!`, then the first physical line of the document is an interpreter directive line. If not, the interpreter directive is absent.

The interpreter directive payload is the content of the first line after the leading `#!`, up to but excluding the terminating newline.

If a document has an interpreter directive and also has a pragma, then the pragma MUST appear after the interpreter directive.

An interpreter directive line is not part of the `children` sequence.

## 8. Pragma

If present, the pragma MUST be the first non-blank line after any interpreter directive line, and is parsed using the ordinary CODL line rules (**P-002**).

If present, the entire pragma line MUST be fully contained within the first 4096 bytes of the document (**P-003**).

The keyword of the pragma line MUST be `pragma`.

The positional form of the pragma is:

```text
pragma 1.0 schema-id #
```

The parameters are interpreted in order as follows:

1. CODL version
2. schema identifier
3. comment marker

The version parameter MUST have the form `x.y`, where `x` and `y` are non-negative integers (**P-004**). `x` is the major version and `y` is the minor version.

The following rules govern how the version number changes across revisions of this specification:

- A revision that accepts documents which would not be accepted by the previous revision MUST increment the major version.
- A revision that accepts a previously accepted document but assigns it a different interpretation in its presentation or semantic model MUST increment the major version.
- A revision that rejects a document that would have been accepted by an earlier revision MUST keep the same major version and increment the minor version.

The schema identifier parameter is optional.

The comment marker parameter is optional.

If the comment marker parameter is present, the schema identifier parameter MUST also be present (**P-005**).

The comment marker MUST be a single ASCII symbolic character. It MUST NOT be SPACE, LINEFEED, CARRIAGE RETURN, a letter, a control character or a digit (**P-006**).

The default comment marker is `#`, used unless the pragma or the document schema specifies a different one.

### 8.1 Schema Identifier

The schema identifier, if present, MUST be one of:

- an HTTP or HTTPS URL, optionally with a fragment (the `#` separator and everything after it) that is the BASE64-URL-encoded (no padding) SHA-256 hash of the BCODL representation of the schema
- a bare BASE64-URL-encoded (no padding) SHA-256 hash of the BCODL representation of the schema

The `#` used in the URL form is the standard URI fragment separator (RFC 3986 §3.5). A bare hash is distinguished from a URL by the absence of a `://` substring. Because the BASE64-URL alphabet contains no space characters, a schema identifier always occupies a single word.

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

A schema identifier that carries a hash takes precedence for matching purposes: a URL-only identifier and a URL-with-hash identifier for the same URL do not automatically match (the hash is authoritative).

FIXME: The definition of schema compatibility (for the compatible-but-not-matching case) is not yet specified (see §20).

If a schema URL is specified but the schema cannot be retrieved, it is a runtime error.

### 8.3 Comment Marker Resolution

The configured comment marker is determined in the following order of increasing precedence:

1. The default marker (`#`)
2. The comment marker declared by the resolved schema, if any
3. The comment marker specified in the pragma, if present

The comment marker MUST be determined before parsing any content after the pragma line. If the effective comment marker requires the schema (i.e., the pragma does not specify a comment marker and the schema may declare one), the parser MUST resolve the schema before continuing.

The comment marker declared by a schema is given by `Schema.marker` (§20).

The pragma line is not included in the `Document.children` sequence. It is recorded only in the `Document.pragma` field.

## 9. Lines, Margin, and Indentation

A CODL document MAY begin with zero or more blank lines.

A document containing no non-blank lines is invalid (**P-007**).

If the document begins with an interpreter directive, the **margin** is zero. Otherwise, the **margin** is the number of leading spaces on the first non-blank line.

Every non-blank line in the document MUST begin with at least the margin number of spaces. A non-blank line beginning with fewer than the margin number of spaces is invalid (**P-008**).

For each non-blank line:

- let `leadingSpaces` be the number of initial spaces on the line
- let `relativeSpaces = leadingSpaces - margin`
- the line indent is `relativeSpaces / 2`

`relativeSpaces` MUST be even (**P-009**). Therefore, after removing the margin, indentation is measured in units of two spaces.

The first non-blank line necessarily has indent `0`.

Trailing spaces on a non-blank ordinary line are not permitted (**P-010**).

Blank lines have no structural effect, except where explicitly stated for tabulated blocks, source atoms, or literal atoms.

## 10. Words and Parameters

Each non-blank ordinary line contains one or more words.

The first word on a non-blank ordinary line is the **keyword**.

Each subsequent word on that line is an inline atom value.

### 10.1 Keyword Characters

A keyword may contain any Unicode code point except U+0020 SPACE and U+000A LINE FEED. Non-printing code points are NOT RECOMMENDED in keywords but are not forbidden.

### 10.2 Parameter Characters

An inline atom value may contain any Unicode code point other than newline, subject to the CODL word-separation rules and any special parsing rules for remarks.

### 10.3 Word-Separation Rule

After the keyword, the line is parsed left-to-right.

Initially, a single space is sufficient to terminate a word and begin the next word.

If a hard space is encountered anywhere after the keyword, then from that point onward only hard spaces terminate words. Soft spaces after that point become part of the current word.

Accordingly:

- before the first hard space on a line, either a soft space or a hard space terminates a word
- after the first hard space on a line, only a hard space terminates a word
- after the first hard space on a line, a soft space becomes content within the current word
- each new line resets this rule

Consequently, after the first hard space on a line, a word may contain soft spaces but may not contain hard spaces.

The remark rule (§11.3) is subject to the word-separation mode in effect at the point it is tested. In soft-space mode, the comment marker preceded by a soft space is a remark introducer. In hard-space mode, the comment marker preceded by a soft space is not at a word boundary and is therefore not a remark introducer; it must be preceded by a hard space to be recognized as one.

## 11. Comments, Tabulations, and Remarks

CODL distinguishes between:

- a **comment**, which occupies an entire line and is represented as a line-level presentation node,
- a **tabulation**, which occupies an entire line and represents the start of a tabulated block, and
- a **remark**, which is attached to a compound line and is not an ordinary child node.

The **configured comment marker** is determined by the resolution rules in §8.3.

### 11.1 Comment

A line is a comment line if, after its leading indentation, its keyword is exactly equal to the configured comment marker, and the line does not qualify as a tabulation line (§11.2). A line qualifies as a tabulation line if at least one further occurrence of the configured comment marker appears on the line preceded by a hard space; in that case the line is a tabulation line and not a comment line, regardless of any other content.

If the comment marker is followed immediately by the end of line, the comment payload is the empty string.

If the comment marker is followed by a soft space, the comment payload begins at the first character after that soft space and continues unchanged to the end of the line.

A word such as `#foo` is never a comment marker.

The payload of a comment is not further parsed. Spaces inside the payload are preserved exactly.

Comments participate in indentation and structural ordering as line-level nodes. Comments cannot have children.

A comment line MUST be immediately preceded by a blank physical line, unless it is the first non-blank physical line of the children sequence (i.e., the first non-blank physical line after any interpreter directive and pragma) (**P-011**). Because a blank line terminates any active tabulated block, this rule ensures that comments cannot appear inside tabulated blocks.

A comment is **attached** to the immediately following node if there is no blank line between the comment and that node. The following node may be a compound node, or a tabulation line (in which case the comment is attached to the tabulated block that the tabulation line introduces). The attached node MUST be at the same indentation level as the comment. A comment that is followed by a blank line, by end of input, or by a line at a shallower indentation level is **free-standing**.

Comment attachment is a semantic property recorded in the presentation model. It is significant during programmatic editing: when a node is moved or deleted, its attached comments travel with it or are removed with it.

### 11.2 Tabulation

A line is a tabulation line if, after its leading indentation, its first non-space character is the configured comment marker, and at least one further occurrence of the configured comment marker appears on the line immediately preceded by a hard space.

Each marker occurrence on a tabulation line is identified as follows: the first non-space character (M_0) is always a marker; any subsequent occurrence of the configured comment marker that is immediately preceded by a hard space is also a marker (M_1, M_2, …).

A tabulation line is represented as a distinct presentation node. The remark rule (§11.3) does not apply to tabulation lines; any content on a tabulation line that would otherwise form a remark is instead part of the heading text for the final column.

The markers on a tabulation line are ordered by position. Let their character offsets from the start of the line, after removing the document margin, be M_0 < M_1 < ... < M_n, where n ≥ 1. The first marker (at M_0) is the line's keyword and carries no column semantics. Each subsequent marker defines a column of the tabulated block: marker at M_i (for i = 1, ..., n) defines the start of **column i**. Columns are numbered from 1.

For each non-final column i (where 1 ≤ i < n), its maximum content width is M\_{i+1} − M_i − 2 code points. The final column (i = n) is unbounded.

**Column headings.** Each marker M_i is followed by a **column heading**, parsed as follows:

- If M_i is immediately followed by end of line, the heading is the empty string.
- If M*i is immediately followed by exactly one space (a soft space), the heading is the text beginning after that space and ending immediately before the first hard space encountered, or at end of line if no hard space follows. If the heading ends at a hard space, the character immediately after that hard space MUST be the configured comment marker (i.e., M*{i+1}) (**P-022**). The heading text MUST NOT itself contain the configured comment marker (**P-022**).
- If M*i is immediately followed by two or more spaces (a hard space), the character immediately after those spaces MUST be the configured comment marker (i.e., M*{i+1}), and the heading for M_i is the empty string (**P-022** if not).
- Any other character immediately following M_i (including a non-space character) is invalid (**P-022**).

The column heading for M_0 labels the keyword and pre-column area of rows. The column heading for M_i (i ≥ 1) labels column i and is positioned within column i's span on the tabulation line.

Column headings are preserved in the `Tabulation` node as an ordered list parallel to `markerOffsets`. An empty string heading is permitted.

Examples:

- `# ID  # Name  # Age` — three markers; headings `["ID", "Name", "Age"]`
- `#  # Name  # Age` — M_0 followed by hard space then M_1; headings `["", "Name", "Age"]`
- `# ID  #  # Age` — M_1 followed by hard space then M_2; headings `["ID", "", "Age"]`
- `# foo  # # bar` — invalid (**P-022**): heading for M_1 would contain the marker
- `# foo  #  bar  # baz` — invalid (**P-022**): M_1 followed by hard space not immediately preceding a marker

### 11.3 Remark

A remark is attached to the compound defined by its line.

A remark begins when the configured comment marker appears at the start of a word and is immediately followed by exactly one soft space. Whether a given occurrence of the comment marker is at the start of a word depends on the word-separation mode in effect at that point (§10.3).

Accordingly:

- the configured comment marker followed by end of line is an ordinary word, not a remark introducer
- the configured comment marker followed by a hard space is an ordinary word, not a remark introducer
- the configured comment marker not preceded by a word boundary in the current mode is ordinary content within the current word
- the configured comment marker at a word boundary followed by a soft space introduces a remark

The remark payload begins at the first character after that soft space and continues unchanged to the end of the line.

The configured comment marker itself is not part of the remark payload.

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

type Atom = Word | Source | Literal;

interface Word {
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

In the semantic model, all atom presentation forms are just atoms, and the distinction between atom and compound disappears in favor of typed nodes with types.

A `Block` is the primary structural grouping within a compound's children. It consists of:

- zero or more **attached comments** (each preceded by a blank line, per §11.1)
- an optional **tabulation** line, which applies to all compounds in the block
- zero or more **compound children** (rows if the tabulation is present, ordinary children otherwise)
- a count of **trailing blank lines**: the number of blank lines that follow the last compound (or, if compounds is empty, the last comment) in the block

A block whose `compounds` list is empty and whose `comments` list is non-empty represents a free-standing comment group (a comment or comments not immediately followed by any compound at the same level).

Each `Atom.Word` records the number of spaces immediately preceding it on its source line.

## 13. Compound Tree Structure

Each non-comment non-tabulation ordinary line defines a `Compound` node whose keyword is the line keyword.

Each inline word after the keyword defines an `Atom.Word` attached to that compound, unless superseded by the remark rule.

After its inline atoms, a compound may have zero or more child blocks (§12), determined by indentation and blank-line structure.

## 14. Parent, Child, and Peer Relations

For each non-blank line after the first non-blank line, excluding lines consumed by source atoms or literal atoms:

- if its indent is exactly one greater than that of the immediately preceding non-blank line, it is a child of the immediately preceding non-blank line
- if its indent is equal to that of the immediately preceding non-blank line, it is a peer of the immediately preceding non-blank line
- if its indent is less than that of the immediately preceding non-blank line, it closes one or more open compounds and becomes a peer of the nearest preceding line with the same indent; if no preceding line has the same indent, the document is invalid (**P-012**)

A line may not have indent greater than one plus the indent of the immediately preceding non-blank line, except where the source-atom or literal-atom rules apply (**P-013**).

Comments and tabulations follow the same indentation and peer/child rules as compounds during parsing, except that comments and tabulations cannot have children. A line that would become a child of a comment or tabulation is invalid (**P-014**). In the resulting presentation model, comments and tabulations are absorbed into `Block` nodes and do not appear as standalone siblings of compounds.

## 15. Source Atoms

If a line immediately follows a compound line with no intervening blank line, and its indent is exactly two greater than that compound line's indent, then it begins a source atom, provided:

- the preceding compound does not already have a source atom or literal atom

A source atom is represented in the presentation model as `Atom.Source(text)` and is appended to the end of the atom sequence of the immediately preceding compound.

A compound may have at most one source atom. Introducing a source atom when the preceding compound already has a source or literal atom is invalid (**P-015**).

The source atom begins on the double-indented line and includes that line together with each subsequent line until either:

- the end of the document is reached, or
- a non-blank line is encountered whose indent is less than the indent of the first source-atom line

Blank lines are permitted within a source atom.

For each non-blank captured line, exactly the indentation of the first source-atom line is stripped from the start of the line. Any surplus leading spaces are preserved.

For each captured line, trailing spaces are stripped. (Source-atom lines are not ordinary lines, so P-010 does not apply to them; trailing spaces are silently removed rather than being an error.)

A blank line within a source atom contributes exactly a newline character to the source-atom payload, regardless of how many spaces it physically contains.

The source-atom payload is otherwise captured literally. In particular, the configured comment marker has no special meaning inside a source atom.

The source-atom payload always ends with exactly one final newline character.

Source-atom lines are not compounds and are never members of a tabulated block. A source atom always terminates any surrounding tabulated block.

After a source atom ends, parsing resumes normally. The next non-source-atom line is evaluated for indentation relative to the compound that introduced the source atom, as if the source atom lines were not present.

## 16. Literal Atoms

If a line immediately follows a compound line with no intervening blank line, and its indent is exactly three greater than that compound line's indent, then it begins a literal atom, provided:

- the preceding compound does not already have a source atom or literal atom

A literal atom is represented in the presentation model as `Atom.Literal(text)` and is appended to the end of the atom sequence of the immediately preceding compound.

A compound may have at most one literal atom. Introducing a literal atom when the preceding compound already has a source or literal atom is invalid (**P-016**).

The opening literal-atom line is not part of the payload.

The remainder of that opening line, from its first content character up to but excluding its terminating newline, is the delimiter.

The delimiter MUST consist only of ASCII characters other than space and newline.

If the delimiter is empty, the line does not begin a literal atom.

The literal payload begins immediately after the newline terminating the delimiter line.

The payload continues verbatim until a subsequent physical line whose entire line is exactly equal to the delimiter.

The closing delimiter line MUST be followed by a newline.

The closing delimiter line is not part of the payload.

The single newline immediately preceding the closing delimiter line is also not part of the payload.

Accordingly, an empty literal payload is permitted.

The literal payload preserves leading spaces, trailing spaces, internal spaces, and all other content exactly.

If the end of file is reached before a closing delimiter line is encountered, the document is invalid (**P-017**).

The configured comment marker has no special meaning inside a literal atom.

The line-ending mode rules of §4 do not apply inside a literal atom payload. `CR` characters within the payload are preserved exactly as-is and carry no special meaning; only the `LF` that terminates each payload line is recognised as a line separator.

Literal atom payload lines are raw: they are not subject to any CODL parsing rules. Indentation, trailing spaces, and all other content are preserved exactly. The only termination condition is the occurrence of a line whose entire content is exactly equal to the delimiter.

Literal-atom lines are not compounds and are never members of a tabulated block. A literal atom always terminates any surrounding tabulated block.

After the closing delimiter line and its terminating newline, parsing resumes normally. The next non-literal-atom line is evaluated for indentation relative to the compound that introduced the literal atom, as if the literal atom lines were not present.

## 17. Tabulated Blocks

A **tabulated block** begins immediately after a tabulation line and continues through each subsequent non-blank line until a blank line is encountered or the end of the document is reached. Lines within a tabulated block (other than the tabulation line itself) are called **rows**.

In the presentation model, a tabulated block is represented as a `Block` whose `tabulation` field holds the tabulation line and whose `compounds` list holds the rows.

A second tabulation line appearing within a continuous run of rows (without an intervening blank line) terminates the current `Block` and begins a new `Block` with the new tabulation. The new block's `trailingBlankLines` on the preceding block is zero, indicating that no blank lines separate the two tabulated sub-blocks.

Every non-blank, non-comment row MUST be an ordinary compound line. Every row MUST have the same indent as the tabulation line (**P-018**). Rows MUST NOT have child line-nodes (**P-014**).

**Row structure.** Each row consists of a keyword and zero or more **pre-column parameters**, followed by zero or more **column values**. The keyword and pre-column parameters are separated from each other by single spaces. Column values are introduced by the column positions defined by the tabulation line.

**Spacing constraints.** The following two rules govern the spacing on every row:

1. Every contiguous run of two or more space characters (a hard space) MUST end at position M_i − 1 for some column i that is present on the row (**P-019**).
2. No two consecutive space characters may appear at any other position on the row (that is, within the keyword, within pre-column parameters, or within a column value) (**P-020**).

These rules together imply:

- the keyword and pre-column parameters are separated from each other by exactly one space
- before each present column i there is exactly one hard space run, ending at M_i − 1
- column values contain no internal consecutive spaces

**Column presence and values.** Column i is **present** on a row if the row contains space characters at both position M_i − 2 and position M_i − 1 (the mandatory minimum for the hard-space separator). Column i is **absent** from a row if the row ends before reaching position M_i − 2; a row need not specify all columns and may omit any suffix of columns.

A present column has an **empty value** if position M_i is itself a space character or the row ends at position M_i − 1. An empty value requires that the subsequent column (if any) is also present, since otherwise the separator spaces at M_i − 2 and M_i − 1 would be trailing spaces, which are not permitted. A row MUST NOT have trailing spaces (**P-010**).

**Width constraint.** For each present non-final column i, its value MUST NOT exceed M\_{i+1} − M_i − 2 code points in width (**P-021**). The final column is unbounded.

**Remarks.** Remarks are permitted on rows. The hard space that introduces a remark, and the remark payload itself, are exempt from the column spacing constraints and are not subject to column-width limits.

If a row violates any of these constraints, the document is invalid (see **P-018** through **P-021**).

## 18. Presentation Model and Semantic Model

CODL defines both a presentation model and a semantic model.

### 18.1 Presentation Model

The presentation model is the direct result of parsing CODL text.

It preserves:

- the optional interpreter directive
- the optional pragma
- compounds and their keywords, atoms, and remarks
- the block structure: for each block, its attached comments, its optional tabulation, its ordered compound children, and its trailing blank line count
- atom presentation form (`Word`, `Source`, or `Literal`)
- for each word atom, the number of spaces immediately preceding it
- ordering and structure derived from indentation

A conforming serializer MUST produce output that, when parsed, yields an identical presentation model. Specifically, the serialized output MUST preserve:

- all compounds, with their keywords, atoms, remarks, and children
- all blocks, with their comments, tabulations (including marker offsets and headings), compounds, and `trailingBlankLines` counts
- for each `Word` atom, the `precedingSpaces` count
- document-level fields: interpreter directive, pragma, and children order

A serializer MAY normalize blank line content to empty lines (rather than space-only lines), and MAY use a minimum hard space (exactly two spaces) before remark introducers, since neither is recorded in the presentation model. All other presentation-model details MUST be reproduced exactly.

### 18.2 Semantic Model

The semantic model is derived from the presentation model using a schema.

The semantic node structure is:

```typescript
interface Node {
  type: NodeType;
  children: Node[];
}
```

FIXME: The exact definition of `NodeType` is not yet specified. It references a schema and uniquely determines the type of the node.

The interpreter directive and pragma are not part of the semantic model.

Comments, tabulations, and remarks are not part of the semantic model.

There is a one-to-one mapping between presentation-layer atoms and compounds, on the one hand, and semantic nodes, on the other hand.

Accordingly:

- every presentation-layer atom maps to exactly one semantic node
- every presentation-layer compound maps to exactly one semantic node
- comments do not map to semantic nodes
- tabulations do not map to semantic nodes
- remarks do not map to semantic nodes

When the presentation model is translated to the semantic model, the schema is used to translate each atom and compound into a node and to ascribe a type to that node.

For the purposes of this document, the application of the schema may be treated as an opaque transformation.

The rules by which types are ascribed are deterministic but may be complex.

The schema language and type assignment algorithm are defined in §20. The exact definition of `NodeType` is outside the scope of the current draft.

## 19. Schema-Governed Structure and Error Diagnosis

In addition to parsing errors, a CODL document may be structurally invalid with respect to a schema.

Parsing determines the presentation model. A schema then determines whether the presentation model can be translated into a valid semantic model.

### 19.1 Atom and Compound Interchangeability

Every presentation-layer atom and every presentation-layer compound corresponds to a semantic node, and every semantic node has a type. The distinction between atom and compound is therefore presentational rather than semantic.

A child whose type can be uniquely inferred from schema position may be written either as an atom in parameter position or as a compound with an explicit keyword. A child whose type cannot be uniquely inferred must be written as a compound with an explicit keyword.

Conversely, when a child is written as a compound with an explicit keyword, an implementation may determine that the same child could have been written positionally as an atom, provided the schema would have assigned the same type deterministically.

### 19.2 Positional Assignment of Parameters

For a given parent type, the schema defines an ordered sequence of child specifications.

The order in which child types are specified in the schema determines the order in which positional parameters are interpreted.

Inline atom parameters may be assigned types from that ordered sequence so long as the applicable child specifications are non-repeatable.

If a parameter position is assigned to a repeatable child type, then all subsequent positional parameters on that same compound line must be assigned to that same repeatable child type.

For a `repeatable` member, occurrences may be split across both of the following:

- inline atoms on the parent compound line, and
- subsequent compound children of the parent with the same keyword

These two assignment mechanisms may be combined freely. The V-009 contiguity rule (§19.3) already prohibits differently-typed compound children from being interleaved between such occurrences. Remark lines do not affect this rule.

### 19.3 Error Taxonomy

Errors are identified by a code of the form **P-NNN** (parsing), **S-NNN** (schema), or **V-NNN** (validation). Parsing errors indicate violations of the presentation-model syntax. Schema errors indicate a malformed schema. Validation errors indicate that a document does not conform to its schema.

Each error is referenced by code at the point in the specification where its trigger condition is defined.

#### Parsing Errors (P-)

| Code  | Section  | Description                                                                                                                                                                                                         |
| ----- | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| P-001 | §4       | BOM present at start of document                                                                                                                                                                                    |
| P-002 | §8       | Pragma is not the first non-blank line after any interpreter directive                                                                                                                                              |
| P-003 | §8       | Pragma line extends beyond the first 4096 bytes                                                                                                                                                                     |
| P-004 | §8       | Pragma version parameter does not have the form `x.y` with non-negative integers                                                                                                                                    |
| P-005 | §8       | Pragma comment marker present without a schema identifier                                                                                                                                                           |
| P-006 | §8       | Pragma comment marker is a space, newline, letter, or digit                                                                                                                                                         |
| P-007 | §9       | Document contains no non-blank lines                                                                                                                                                                                |
| P-008 | §9       | Non-blank line begins with fewer than the margin number of spaces                                                                                                                                                   |
| P-009 | §9       | Relative indentation after the margin is odd                                                                                                                                                                        |
| P-010 | §9, §17  | Trailing spaces on a non-blank ordinary line or tabulated row                                                                                                                                                       |
| P-011 | §11.1    | Comment line not immediately preceded by a blank line                                                                                                                                                               |
| P-012 | §14      | Line indent is less than the preceding non-blank line's indent and no ancestor has the same indent                                                                                                                  |
| P-013 | §14      | Line indent exceeds the preceding non-blank line's indent by more than one                                                                                                                                          |
| P-014 | §14, §17 | Line would become a child of a comment, tabulation, or tabulated row                                                                                                                                                |
| P-015 | §15      | Source atom introduced when the preceding compound already has a source or literal atom                                                                                                                             |
| P-016 | §16      | Literal atom introduced when the preceding compound already has a source or literal atom                                                                                                                            |
| P-017 | §16      | Literal atom reaches end of file before its closing delimiter line                                                                                                                                                  |
| P-018 | §17      | Tabulated row has an indent different from the tabulation line                                                                                                                                                      |
| P-019 | §17      | Hard space on a tabulated row does not end at a column start boundary                                                                                                                                               |
| P-020 | §17      | Consecutive spaces appear within a keyword, pre-column parameter, or column value on a tabulated row                                                                                                                |
| P-021 | §17      | Column value exceeds the maximum width for that column                                                                                                                                                              |
| P-022 | §11.2    | Malformed tabulation line heading: marker followed by hard space not immediately preceding another marker; heading text contains the configured comment marker; or non-space character immediately follows a marker |
| P-023 | §4       | `CR` not immediately followed by `LF`, or line-ending mode is inconsistent with the mode established by the first `LF` in the document                                                                              |

FIXME: The following parsing error conditions are known to be missing from this table and will be added in a future revision: tabulation line with fewer than two markers.

#### Schema Errors (S-)

| Code  | Section | Description                                                                                                                       |
| ----- | ------- | --------------------------------------------------------------------------------------------------------------------------------- |
| S-001 | §20.1   | `Schema.document` is not a `Struct`                                                                                               |
| S-002 | §20.1   | Duplicate keyword within the bindings of a single `Struct`                                                                        |
| S-003 | §20.1   | `Member` has an empty `bindings` list                                                                                             |
| S-004 | §20.1   | `Member` has more than one binding and not all bindings are `Flag`                                                                |
| S-005 | §20.2   | Root struct has a `required` atom-assignable member (unreachable: the document root has no atoms)                                 |
| S-006 | §20.1   | `Primitive` has a non-null `default` but appears in a non-`required` member                                                       |
| S-007 | §20.3   | Two or more `Layer`s within a `Schema` share the same `id`                                                                        |
| S-008 | §20.3   | A `Layer` introduces a coproduct member whose keyword set overlaps with an existing keyword in the base `Struct`                  |
| S-009 | §20.3   | A `Layer` member matches an existing keyword in the base `Struct` but the base type or the layer type (or both) is not a `Struct` |
| S-010 | §20     | `Schema.marker` is non-null and is a space, newline, letter, or digit                                                             |

#### Validation Errors (V-)

| Code  | Section | Description                                                                                                                                                              |
| ----- | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| V-001 | §20.2   | Compound's type is not a `Struct`                                                                                                                                        |
| V-002 | §20.2   | More atoms on a compound than there are assignable member positions                                                                                                      |
| V-003 | §20.2   | Atom appears at a member position that is not atom-assignable                                                                                                            |
| V-004 | §20.2   | Atom text matches no `Flag` keyword among a multi-binding `Flag` member's bindings                                                                                       |
| V-005 | §20.2   | Atom text does not match the single `Flag` binding's keyword                                                                                                             |
| V-006 | §20.2   | Compound keyword is not recognized for its parent type                                                                                                                   |
| V-007 | §20.2   | Required member has no atoms or compound children assigned to it, and the member's single binding's type is not a `Primitive` with a non-null `default`                  |
| V-008 | §20.2   | Non-repeatable member is filled more than once                                                                                                                           |
| V-009 | §20.2   | Compound children of the same member are not contiguous (a child of member i appears, then one or more children of a different member j, then another child of member i) |

FIXME: The following validation error conditions are known to be missing from this table: `Primitive` validator failure; `Flag` compound with parameters or children.

### 19.4 Error Diagnosis

Error diagnosis in CODL has at least two layers:

- **parsing diagnosis**, which reports violations of the presentation syntax defined by this specification
- **schema diagnosis**, which reports violations that arise when the presentation model is checked against a schema and translated into the semantic model

These two layers SHOULD be distinguished in diagnostics.

A conforming implementation SHOULD report multiple independent errors when validating a document, rather than halting at the first error encountered. Each error has a defined recovery strategy (§19.5) that allows parsing to continue and subsequent errors to be reported.

FIXME: The required form, precision, and source-location behavior of diagnostics are not yet specified.

### 19.5 Error Recovery

For every error condition, a conforming implementation MUST apply the recovery strategy defined here before continuing. No error SHALL prevent subsequent errors from being reported.

#### Parsing Error Recovery

| Code  | Recovery strategy                                                                                                                                                                                                                                                                                                                   |
| ----- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| P-001 | Ignore the BOM and continue parsing from the next byte.                                                                                                                                                                                                                                                                             |
| P-002 | Restart parsing the entire document using the version, schema identifier, and comment marker extracted from the misplaced pragma.                                                                                                                                                                                                   |
| P-003 | Allow the pragma line to exceed the 4096-byte limit and continue parsing its content normally.                                                                                                                                                                                                                                      |
| P-004 | If the version parameter cannot be parsed as `x.y` at all, ignore it and parse with the latest known version. If it has the correct format but names an unknown version, use the most recent minor version of the same major version if one is known; if the major version itself is unknown, use the latest known version overall. |
| P-005 | Treat the comment marker as valid even though no schema identifier is present; use it as the configured comment marker.                                                                                                                                                                                                             |
| P-006 | Ignore the invalid comment marker and use the default marker (`#`) instead.                                                                                                                                                                                                                                                         |
| P-007 | Terminal. No recovery is possible or necessary; parsing cannot continue on an empty document.                                                                                                                                                                                                                                       |
| P-008 | If the line has exactly one fewer leading space than the current margin, insert a synthetic leading space and parse the line at the current indentation level normally. If the line has two or more fewer leading spaces than the current margin, reset the margin to the line's actual indentation level from that point forward.  |
| P-009 | FIXME: See general note on indentation recovery below.                                                                                                                                                                                                                                                                              |
| P-010 | Ignore trailing spaces and parse the remainder of the line normally.                                                                                                                                                                                                                                                                |
| P-011 | Ignore the absence of a preceding blank line and treat the comment as normally attached to the following node.                                                                                                                                                                                                                      |
| P-012 | FIXME: See general note on indentation recovery below. (This condition is closely related to P-009: there should always be a matching ancestor node at every even dedent depth, but not necessarily at every odd depth.)                                                                                                            |
| P-013 | FIXME: See general note on indentation recovery below.                                                                                                                                                                                                                                                                              |
| P-014 | FIXME: See general note on indentation recovery below.                                                                                                                                                                                                                                                                              |
| P-015 | Ignore the duplicate source atom; use the first one encountered.                                                                                                                                                                                                                                                                    |
| P-016 | Ignore the duplicate literal atom; use the first one encountered.                                                                                                                                                                                                                                                                   |
| P-017 | Treat the unclosed literal atom's payload as everything from the opening delimiter line to the end of file (excluding the final newline, if any).                                                                                                                                                                                   |
| P-018 | Interpret the tabulated row according to its actual hard-space positions regardless of alignment with column markers. Suppress any further alignment errors (P-019, P-020, P-021) on the same row.                                                                                                                                  |
| P-019 | Same as P-018.                                                                                                                                                                                                                                                                                                                      |
| P-020 | Same as P-018.                                                                                                                                                                                                                                                                                                                      |
| P-021 | Same as P-018.                                                                                                                                                                                                                                                                                                                      |
| P-022 | Report the error and continue parsing, but disable column-alignment checking for the remainder of the current tabulated block.                                                                                                                                                                                                      |
| P-023 | Treat any malformed sequence of consecutive `CR` and `LF` characters as a single line break if it contains at most one `CR` and at most one `LF`; treat it as two line breaks if either `CR` or `LF` appears more than once in the sequence.                                                                                        |

#### Validation Error Recovery

All validation errors (V-001 through V-009) are self-contained: they do not have cascading effects on the remainder of the type assignment algorithm. An implementation MUST record the error and continue processing remaining nodes as if the erroneous node were absent or were assigned the most plausible available type.

#### General Note on Indentation Recovery

The recovery strategies for P-009, P-012, P-013, and P-014 are deferred because they depend on information that may not be available until after the malformed line is encountered. In general, an off-by-one indentation error (one space too many or one space too few relative to a valid indent level) is recoverable: the malformed line SHOULD be held in a provisional **malformed node** and the parser SHOULD read ahead to determine whether the line should be treated as:

- indented one level deeper than its parent (the extra space is synthetic noise),
- at the same level as its parent (the missing space is synthetic noise), or
- the start of a new effective margin (all subsequent lines share the same odd indentation).

The resolution among these interpretations is deterministic but depends on the structure of subsequent lines and, where a schema is available, on the types expected at each level. The precise algorithm is not yet specified.

FIXME: The exact indentation recovery algorithm for P-009, P-012, P-013, and P-014 is not yet specified.

## 20. Schema Language

A schema is expressed using the following types:

```typescript
interface Schema {
  id: string;
  document: Type;
  layers: Layer[];
  marker: string | null;
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

interface Member {
  required: boolean;
  repeatable: boolean;
  bindings: Binding[];
}

interface Binding {
  keyword: string;
  type: Type;
}
```

`Schema.id` is an ASCII string identifier for the schema. It MUST consist only of printable ASCII characters other than whitespace.

`Schema.document` is the type of the document root. It MUST be a `Struct`.

`Schema.layers` is an ordered list of `Layer` values defining optional schema extensions. The empty list is the normal case for a schema with no layers. Layer composition is defined in §20.3.

`Schema.marker` is the default comment marker character for documents that use this schema, or `null` if the schema does not declare a preferred marker. When non-null, it MUST satisfy the same character constraints as a pragma comment marker (§8): it MUST NOT be a space, newline, letter, or digit (**S-010**). When a document's pragma omits a comment marker but provides a schema identifier that resolves to a schema with a non-null `marker`, the resolved marker is used as if it had been specified in the pragma (§8.3).

CODL schemas are themselves representable as CODL documents. The CODL schema that describes the CODL schema language is therefore self-describing; the schema for schemas is identified by the schema ID `codl-schema`. The serialization of a schema as a CODL document is governed by that schema.

A `Struct` is a product type: it has an ordered list of `Member`s. Each `Member` describes one logical child slot of the struct, with the following properties:

- `required`: if `true`, the slot MUST be present at least once in a conforming document
- `repeatable`: if `true`, the slot MAY appear more than once; if `false`, it MUST appear at most once
- `bindings`: a non-empty list of `Binding`s, each associating a keyword with a type

If a member has more than one binding, any binding's keyword may be used to fill that slot; the chosen keyword determines the type of the child node placed in that slot.

`Binding.keyword` is the keyword by which a child compound of that type is written in CODL when explicit.

`Binding.type` may be any `Type`.

A `Primitive` type represents a leaf value constrained by a validator. `Primitive.validator` names the validator to be applied to the atom text. `Primitive.default` is either `null` (no default) or a string giving the value to be used when the member is absent. A non-null default MAY only be specified if the `Primitive` appears in a `required: true` member; specifying a non-null default on a non-required member is a schema error (**S-006**). When a required member whose single binding has a `Primitive` type with a non-null default is absent from the document, the default value is used as the semantic value and no V-007 error is raised.

A `Flag` type carries no value of its own. Its identity is entirely determined by `Binding.keyword`: in compound position, a `Flag`-typed node is written as the keyword alone, with no parameters; in atom position, the atom text is matched against `Binding.keyword`. A `Flag`-typed member SHOULD NOT be `required`, since a required `Flag` member would be unconditional boilerplate.

### 20.1 Schema Validity Constraints

A schema is invalid if any of the following holds:

- `Schema.document` is not a `Struct` (**S-001**)
- within the `bindings` of any single `Struct`, the same keyword appears more than once across all members' bindings (**S-002**)
- any `Member` has an empty `bindings` list (**S-003**)
- a `Member` has more than one binding and at least one of those bindings is not `Flag` (**S-004**)
- a `Primitive` has a non-null `default` and appears in a `Member` with `required: false` (**S-006**)
- `Schema.marker` is non-null and is a space, newline, letter, or digit (**S-010**)

### 20.2 Type Assignment Algorithm

Type assignment translates the presentation model into the semantic model by ascribing a type to every atom and compound node in the tree. It proceeds as a recursive descent over the tree, guided by the schema.

**Atom-assignable members.** A member M is _atom-assignable_ if every binding B in M.bindings has `B.type` equal to `Primitive` or `Flag`. A member that is not atom-assignable may only be satisfied by compound children (written with an explicit keyword), not by inline atoms. A member with more than one binding is atom-assignable only if all its bindings are `Flag` (a mix of `Primitive` and `Flag` bindings would be ambiguous in atom position).

**Document root.** The document root is a virtual compound node with type `Schema.document`. It has no atoms; any members of the root struct that are atom-assignable but `required` cannot be satisfied (**S-005**).

**Type assignment for a compound node N with type T:**

1. T MUST be a `Struct`; if it is not, the document is invalid (**V-001**).

2. Construct the keyword map K: for each member M at index i in T.members, for each binding B in M.bindings, map B.keyword → (i, B). (Schema validity ensures no duplicate keywords within the same struct.)

3. **Atom phase.** Let `pos` = 0. For each atom A in N.atoms, in order:

   a. Advance `pos` while the following skip condition holds: `pos` < len(T.members), the member at `pos` is not `required`, every binding in that member is `Flag`, and the text of A does not equal the `keyword` of any of those bindings. Each advanced-past member is recorded as absent.

   b. If `pos` ≥ len(T.members), the document is invalid (**V-002**: more atoms than assignable member positions).

   c. Let M = T.members[pos]. M MUST be atom-assignable; if it is not, the document is invalid (**V-003**: atom in non-atom-assignable member position).

   d. Assign A to M. If M has multiple bindings (all `Flag`), the matched binding is the one whose `keyword` equals A's text; if no binding's keyword matches, the document is invalid (**V-004**). If M has a single `Flag` binding, A's text MUST equal B.keyword; if it does not, the document is invalid (**V-005**). If M has a single `Primitive` binding, the type of A is that `Primitive` regardless of A's text (validation against `Primitive.validator` is a separate step).

   e. If M is not `repeatable`, increment `pos`. If M is `repeatable`, leave `pos` unchanged; all subsequent atoms are also assigned to M.

4. **Compound child phase.** Let `current_member` = −1 and `seen_members` = {} (empty set). For each compound child C in N.children (iterating across all blocks in order):

   a. Look up C.keyword in K. If not found, the document is invalid (**V-006**: unrecognized keyword for this parent type).

   b. Let (i, B) = K[C.keyword]. The type of C is B.type.

   c. If i ≠ `current_member`: if i is in `seen_members`, the document is invalid (**V-009**: member children not contiguous); otherwise add `current_member` to `seen_members` (if ≥ 0) and set `current_member` = i.

   d. Record that member at index i has been filled by C.

   e. Recursively apply type assignment to C with type B.type.

5. **Constraint check.** For each member M in T.members:

   a. Let fill_count = (number of atoms assigned to M) + (number of compound children assigned to M).

   b. If M.`required` and fill_count = 0: if M.`bindings` has exactly one binding whose type is a `Primitive` with a non-null `default`, the default value is used as the semantic value and no error is raised; otherwise, the document is invalid (**V-007**: required member absent and no default available).

   c. If not M.`repeatable` and fill_count > 1, the document is invalid (**V-008**: non-repeatable member filled more than once).

### 20.3 Schema Layering

A `Schema` may include one or more `Layer` values in its `layers` list. Each layer describes an incremental extension to the schema's root struct. Layers are append-only: they may add new members to a struct but may not delete existing members or alter the `required` or `repeatable` properties of existing members.

**Composed schema identity.** A composed schema is identified by the base schema's `id` together with the ordered sequence of layer `id`s applied to it. Two schemas with the same base `id` but different layer sequences are distinct schemas.

**Merge algorithm.** The function `Merge(base: Struct, layer: Struct): Struct` produces a new struct that incorporates the layer's members into the base:

1. Begin with a copy of `base.members` in their original order.

2. Construct the keyword map K for the base struct: for each member M at index i in `base.members`, for each binding B in M.bindings, map B.keyword → (i, M).

3. For each member L in `layer.members` in order:

   a. **Single-binding members.** If L has exactly one binding (keyword W, type T):
   - Look up W in K.
   - **Found:** Let (i, M) = K[W]. Let B₀ = M.bindings[0]. Both B₀.type and T MUST be `Struct`; if either is not, the layer is invalid (**S-009**). Replace B₀.type in the merged member list at index i with `Merge(B₀.type, T)`.
   - **Not found:** Append L as a new member at the end of the member list. Add W → (new index, L) to K.

   b. **Multi-binding (coproduct) members.** If L has more than one binding:
   - Every keyword in L.bindings MUST be absent from K; if any keyword in L.bindings already exists in K, the layer is invalid (**S-008**).
   - Append L as a new member at the end of the member list. For each binding B in L.bindings, add B.keyword → (new index, L) to K.

4. Return the resulting member list as the merged struct.

**Layer validity constraints.** A schema is invalid if any of the following holds:

- Two or more layers within the same schema share the same `id` (**S-007**)
- A layer member with more than one binding has any keyword that already appears in the keyword map of the (progressively merged) base struct (**S-008**)
- A layer member with exactly one binding matches an existing keyword, but the base type, the layer type, or both are not `Struct` (**S-009**)

**Composing layers.** To apply a sequence of layers `[L₁, L₂, …, Lₙ]` to a base schema with root struct R, apply `Merge` iteratively: start with R₀ = R, then Rₖ = `Merge(Rₖ₋₁, Lₖ.root)` for k = 1 … n. The final Rₙ is the root struct of the composed schema.

### 20.4 BCODL and Schema Hashing

**BCODL** is the binary encoding of the semantic model of a CODL document. Every well-typed CODL document has exactly one BCODL encoding; the mapping is fully deterministic. A schema is itself a CODL document and therefore has a BCODL encoding.

The **hash** of a schema is the SHA-256 digest of the BCODL encoding of that schema. The hash is represented as a BASE64-URL-encoded (no padding) string of 44 characters.

This hash is used in schema identifiers (§8.1) to verify that the schema in use is the one the document was authored against.

#### 20.4.1 Integer Encoding

All counts and byte-lengths in BCODL are non-negative integers encoded in a variable-length format. To encode an integer N:

1. Set B = N & 0x7F (the seven least-significant bits of N).
2. Set N = N >> 7.
3. If N > 0, set bit 7 of B (i.e. B = B | 0x80) and write the byte B; then repeat from step 1.
4. If N = 0, write B as the final byte (bit 7 is clear).

The result is one or more bytes. Every byte except the last has bit 7 set (a **continuation byte**). The last byte has bit 7 clear. The seven low-order bits of each byte, concatenated from least-significant (first byte) to most-significant (last byte), reconstruct the original integer.

Decoding: read bytes in sequence; for each byte, take bits 0–6 and OR them into the accumulator at the current bit offset; advance the bit offset by 7. If bit 7 of the byte is set, read the next byte; otherwise the integer is complete.

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

Within any struct type, the keywords visible at that level are assigned a **keyword index**: a zero-based integer determined by flattening the struct's member list in declaration order and, within each member, its binding list in declaration order. The first keyword of the first member is index 0; the last keyword of the last member has the highest index. This is the same enumeration order that the keyword map K in §20.2 iterates over when it is constructed; the keyword index is the position of a keyword in that enumeration.

Because the schema determines the type of every node from its keyword index and its parent's type, BCODL encodes no type tags.

#### 20.4.3 File Layout

A BCODL file consists of the following fields in order:

1. **Magic number**: the 5 bytes `42 43 4F 44 4C` (ASCII `BCODL`).
2. **Schema signature**: FIXME — not yet specified.
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

**Default values.** BCODL encodes the semantic model, in which a required `Primitive` member with a non-null default is semantically present even when it was absent from the source document. Therefore, when encoding a document to BCODL, a missing required primitive whose default is used MUST be encoded as a primitive node with the default value string. This ensures that the BCODL encoding is identical regardless of whether the member was explicitly written or filled by its default.

There are no pad bytes, alignment constraints, or inter-node delimiters. The schema provides all type information needed to decode the stream unambiguously.

FIXME: The definition of schema compatibility (compatible-but-not-matching) is not yet specified.

## 21. Reserialization and Editing

The presentation model can be mutated to reflect changes to the semantic model, preserving formatting, comments, tabulations, and remarks wherever possible. Mutations are expressed as operations on the semantic model, which are then reflected in the presentation layer.

There are two categories of editor:

- **Human editors**, who modify source text directly with full flexibility and no constraints on what changes may be made
- **Computer editors** (programmatic transformations), which apply structured operations to the semantic model and reserialize through the presentation layer

### 21.1 Comment Attachment and Editing

Each `Block` in the presentation model carries zero or more attached comments (§11.1) that precede its compounds. These comments travel with the block under programmatic transformations.

When a computer editor deletes a compound, the `Block` that contained it is updated. If the deleted compound was the only compound in its block, and if the block has attached comments, those comments are also removed (since their meaning was associated with that block).

When a computer editor moves a compound, its containing block's structure is preserved where possible: if the move results in the block having no remaining compounds, the block (and any attached comments) moves with the compound to the new location.

When a computer editor inserts a compound constructed from purely semantic information, no comment is attached to it initially; it is placed into an existing block or a new empty block as appropriate.

### 21.2 Computer Editor Operations

A computer editor MUST perform only operations drawn from the following set. Each operation preserves all presentation-layer details that are not directly affected by the operation: remarks, `trailingBlankLines` counts, `precedingSpaces` on word atoms, and tabulation marker offsets are all retained unless the operation explicitly targets them.

**Delete.** Remove a compound that is not `required`. Any remark attached to the compound is removed with it. If the compound's block becomes empty (no remaining compounds), the block and its attached comments are also removed.

**Replace.** Substitute a compound for another of the same schema type at the same position in the same block. The replacement retains the original compound's remark and its position within the block. If the replacement uses a different keyword (a different binding of the same sum-type member), the keyword in the presentation layer is updated accordingly. Attached comments on the block are preserved.

**Construct.** Create a new compound from purely semantic information, with no presentation-layer context. The constructed compound carries no remark. A serializer producing a constructed compound MUST choose a canonical presentation form.

**Insert (natural position).** Insert a compound into the child structure of a parent at the natural position for its member: after all existing compounds of the same member, within the same block if one exists for that member group, or in a new block otherwise.

**Insert (before sibling).** Insert a compound immediately before a specified existing sibling compound. The inserted compound is placed in the same block as the sibling if the block does not have a tabulation, or in a new block immediately before the sibling's block if it does.

**Insert (after sibling).** Insert a compound immediately after a specified existing sibling compound, subject to the same block-placement rules as insert-before-sibling.

**Insert (into block).** Append a compound to the `compounds` list of a specified existing block. This is the natural way to add rows to a tabulated block. The block's tabulation must have sufficient column capacity for the new compound; if not, resize the tabulation first.

**Attach remark.** Add a remark string to a compound. If the compound already has a remark, it is replaced.

**Remove remark.** Remove the remark from a compound.

**Update value.** For a compound or atom whose schema type is `Primitive`, update the atom text to a new string. The new string MUST satisfy the `Primitive.validator`. All other presentation details of the compound are retained.

**Toggle flag.** Add or remove a `Flag`-typed compound within a parent, provided the result satisfies the `required` and `repeatable` constraints for that member.

**Switch binding.** For a compound that fills a multi-binding (sum-type) member, change the binding used. The compound's keyword in the presentation layer is updated to the new binding's keyword and its schema type changes accordingly. This operation is only valid if the new type is structurally compatible with the compound's existing content.

**Reorder within member group.** Change the position of a compound among its siblings within the same member group (i.e., other compounds filling the same schema member). This operation never violates V-009. The reordered compound retains its remark; attached comments on the affected blocks are preserved.

**Reorder member groups.** Change the relative order of two distinct member groups within a parent's child structure, by moving all blocks belonging to one group before or after all blocks belonging to another. This is valid as long as neither group is interleaved with the other (V-009 is satisfied before and after). Attached comments on all affected blocks are preserved.

**Resize tabulation.** Adjust the `markerOffsets` of a block's `Tabulation` to accommodate all current column values and any values about to be added. New offsets MUST be chosen such that every existing and planned column value fits within the column widths defined by §11.2, and MUST be a minimal adjustment (columns are not widened beyond what is required). After resizing, all existing row content MUST be re-padded with spaces to align to the new column positions. The `headings` list is updated in parallel with `markerOffsets`: existing headings are preserved in place and re-padded within their updated column spans; no heading text is added or removed by this operation.

FIXME: The precise rules for canonical presentation form of constructed compounds, the definition of structural compatibility for switch-binding, and the rules for concurrent error reporting during reserialization are not yet specified.

## 22. Invalidity Conditions

A CODL document is invalid if any condition identified by a **P-NNN** or **V-NNN** error code in this specification is triggered. A schema is invalid if any **S-NNN** condition is triggered. The complete taxonomy of all error conditions, their trigger sections, and their recovery strategies are given in §19.3 and §19.5 respectively.

## 23. Deferred Topics

The following topics remain underspecified or unresolved:

- the BCODL schema signature field (§20.4.3; the remainder of BCODL is specified in §20.4)
- the definition of schema compatibility (matching is defined; compatible-but-not-matching is not) (see §20)
- the exact formal definition of `NodeType`
- the exact deterministic mapping procedure from presentation model to semantic model
- the complete schema-error taxonomy and diagnostic rules (S-001 through S-009 defined; further errors may be needed for `Primitive` validator failure, `Flag` compound constraints, etc.)
- the required form, precision, and source-location behavior of diagnostics
- the exact indentation recovery algorithm for P-009, P-012, P-013, and P-014 (see §19.5)
- the treatment of empty strings as Primitive values
- canonical serialization rules, if any
- precise mutation semantics
- examples and a reference parsing algorithm
