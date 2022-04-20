# CoDaLa, a Collaborative Data Language

CoDaLa is a data format for representing structured data, with special features for working with files that may
be edited by both humans and computers. CoDaLa files keep markup syntax to a minimum, with spaces, newlines and
the `#` character being the only significant characters

## Features
- Models tree-structured data
- Minimal markup characters, which is a pleasure to write
- Computer modifications will not reformat a file
- JSON or XML can be easily embedded within CoDaLa
- Lightweight data schemas with simple syntax
- Extensible editable data verification
- Fast binary format (BiCoDaLa)
- Schema compatibility checking
- Support for comments
- Data and schemas are composable

Here is an example of some CoDaLa data:
```codala
import parent
project main
  module alpha
    name         Alpha
    description  This is a description

  # Todo: 
  module beta
    name Beta
    description
        This is a longer description which flows onto
        more than one line.
```

None of the keys such as `import`, `Project` or `name` have meaning to `CoDaLa`. They are just identifiers, and
may be defined in a schema, such as:
```codala-schema
Project id!
  Module id!
    name text
    description text+
```

## Writing CoDaLa

Writing CoDaLa is easy. Each line contains some words, or data, which represent a node in the tree. Each line is
indented by a number of spaces. If the indentation is the same as the previous line, then the node is a sibling
or peer--it shares the same parent node. If it is indented two spaces more than the previous line, then it is a
child.

If the line has less indentation than the previous line, then it is the direct child of an earlier node: the
last one with two fewer spaces of indentation.

Any other indentation, including having an odd number of spaces, is considered an error. There is one exception
to this for supporting multiline strings, which is explained below.

Each data line contains one or more words (or character sequences), separated by spaces. Any number of spaces
may appear between words without any semantic significance, 

Blank lines may appear anywhere. This format has no significant punctuation other than whitespace, and its
visual representation should seem very natural to a human reader.

### Comments

A CoDaLa document may contain human-readable comments. They contain no data, and their contents is not
interpreted but they are part of the CoDaLa metamodel, and can only appear in certain places in a document.

Comments always begin with a `#` character. If appearing on the same line as data, they must be preceded by at
least one space.

For example, the line,
```codala
    email user@example.com # The user's email address
```
would contain the data, `email user@example.com`, and the comment, `The user's email address`, but the line,
```codala
    url https://example.com/page#ref
```
would not contain any comment.

A comment may also appear alone on a line, but whitespace is significant. Standalone comments must not be
indented, and must be followed by at least one blank line. For example,
```codala
# Directory structure:

home
  work
  data
```

The attachment of comments determines how they are handled during automated modification of a CoDaLa document.
If a node is _deleted_ then any attached comments will also be removed. Standalone comments are never deleted.

, and determines whether the comment
is _attached_ to a node.

### Multiline values

Sometimes it is necessary to write a value containing more than one line of text, or which contains spaces,
or the character sequence ` #`, without being considered a comment. This is possible using a _double indent_:
instead of writing a key and its value on the same line, such as,
```codala
dog
  name        Fido
  description furry
```
we can write:
```codala
dog
  name Fido
  description
      Furry, brown and cuddly.
```

A double-indented value continues so long as its indentation level is maintained. Thus, in,
```codala
dog
  name Fido
  description
      Furry, brown
      and cuddly.
```
the value of `description` would be, `Furry, brown\nand cuddly`: the newline character (`\n`) is part of the
value, but the six spaces of indentation are not. Nevertheless, additional spaces may be included:
```codala
  description
      Furry, brown
       and cuddly
```
would be interpreted as a `Furry, brown\n and cuddly`, but any subsequent line with less indentation would
terminate the multiline value, and be interpreted as new data.

This is particularly useful for embedding other languages in CoDaLa. For example,
```codala
data
  representations
    json
        { "name": "Fido", "description": "furry" }
    
    xml
        <dog>
          <name>Fido</name>
          <description>furry</description>
        </dog>

    markdown
        # Dog

        *Fido* is a furry dog.
```

Note, in particular, that since the markdown value, `markdown`, is indented as a multiline value,
`# Dog` is not interpreted as a comment.

### Embedded form

When embedding CoDaLa data in a host language, we often want to add additional indentation so that the
embedded CoDaLa aligns with the surrounding code. When a CoDaLa document is parsed, the indentation of the first
line containing data is noted, and subtracted from all subsequent lines.

For example, in Scala we might write,
```scala
object Data:
  val animal: String = """
    Animal dog
      name Fido
      legs 4
      tail yes
  """
```

It is therefore also possible to interpret any contiguous fragment of a CoDaLa document provided no line
contains less indentation than the first line of data.

From the example at the top of the page, the fragment,
```codala
  module alpha
    name         Alpha
    description  This is a description
```
is itself a valid CoDaLa document.

## Binary form (BiCoDa)

CoDaLa can provide a convenient way of storing or transmitting tree-structured data. But for fast serialization
and deserialization, a binary form exists, as a direct translation of the same data model, which can be written
and read faster, and which uses less memory. This is called BiCoDaLa.

Although CoDaLa is a _binary_ format, in the sense that it is not for human consumption, it contains only valid
printable UTF-8 characters, making it seamless to copy/paste or to embed within other textual data formats, such
as XML or JSON.

BiCoDaLa should use the custom Media Type `application/x-bicoda`. BiCoDa data always begins with the byte
sequence, `b1` `c0` `da`, which looks like `±ÀÚ`.

## Schemas

### Verification
