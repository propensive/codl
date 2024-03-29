<p align="center"><img src="/doc/logo.svg" height="300"></p>

# CoDL, a Collaborative Data Language

CoDL is a format for representing tree-structured data, designed for documents that may
be edited by both humans and computers. CoDL keeps markup to a minimum: spaces and
newlines define structure, while `#` is the only other meaningful character, for starting
a comment.

## Features
- Models tree-structured data
- Symbolic markup is minimal, making it more enjoyable to write
- Automatic modifications don't reformat a document
- Documents may be untyped or typed
- Allows embedded textual content (such as XML or JSON) without escaping
- Lightweight data schemas with simple syntax
- User-extensible data verification
- Fast and lightweight binary format (BiCoDL)
- Safe schema evolution with compatibility checking
- Allows comments, which may be attached to data, or not
- Both data and schemas are composable

Here is an example of CoDL being used to describe a project and modules:
```codl
import parent
project main
  module alpha
    name         Alpha
    description  This is a description

  # Todo: tidy up this section
  
  # Previously called "beta"
  module gamma
    name Gamma
    description
        This is a longer description which flows onto
        more than one line.
```
In this example, the keywords `import`, `project` or `name` are not part of `CoDL`, but may be part of a
schema that defines the data's structure, such as:
```codl-schema
import  ref!
project id!
  module id!
    name value
    description? value&
    links? link+
```
Each line defines a keyword, and gives names (e.g. `ref` or `value`) to its parameters, indicating where it
may appear in the document with indentation. The symbols `!`, `?` and `&` define the multiplicity of a keyword;
details like whether it is required, unique or may be repeated.

## Writing CoDL

Writing CoDL is easy. Each line contains some words, or data, which represent a node in the tree. Each line is
indented by a number of spaces. If the indentation is the same as the previous line, then the node is a sibling
or peer—it shares the same parent node. If it is indented two spaces more than the previous line, then it is a
child.

If the line has less indentation than the previous line, then it is the child of an earlier node: the last one
with two fewer spaces of indentation—exactly as the visual appearance implies.

Any other indentation, including having an odd number of spaces, is considered an error. There is one exception
to this for supporting multiline strings, which is explained below.

Each data line contains one or more words (or character sequences), separated by spaces. Any number of spaces
may appear between words without any semantic significance, 

Blank lines may appear anywhere. This format has no significant punctuation other than whitespace, and it
should seem very natural to a human reader.

### Comments

A CoDL document may contain human-readable comments. They contain no data, and their contents is not
interpreted but they are part of the CoDL metamodel, and can only appear in certain places in a document.

Comments always begin with a `#` character. The `#` must either start a line or be preceded by at least one
space, and must always be followed by one or more spaces.

For example, the line,
```codl
    email user@example.com     # The user's email address
```
would contain the data, `email user@example.com`, and the comment, `The user's email address`, but the line,
```codl
    url https://example.com/page#ref
```
and,
```codl
  reference #foo
```
would not contain any comments.

A comment may also appear alone on a line, but the whitespace around it is significant: it must exist at
a valid indentation level, that is, preceded by an even number of spaces, and up to one level higher than
the previous line.

For example, like this,
```codl
usr
  local
    bin
    
      # This is a valid comment
```
or this,
```codl
usr
  local
    bin
    
  # This is a valid comment
```
but not this,
```codl
usr
  local
    bin
    
          # This is a valid comment
```
or this:
```codl
usr
  local
    bin

 # This is a valid comment
```

Comments are _attached_ to data nodes, and their attachment is determined by the whitespace around them.
Comments will attach to a data node if they appear on the line immediately preceding the data, at the same
level of indentation. If that node is deleted by a computer editor, the comment will be deleted too.

Standalone comments may also be followed by blank line, in which case their are attached to the parent node.
Such comments will be retained even when data nodes around them are modified, but will be removed if the
parent node is deleted.

An uninterrupted sequence of comment lines at the same indentation level is treated as a single comment.

There are two special rules relating to comments on the first line of a CoDL document: if the first line is
a comment (one or more lines long), then it _must_ be followed by a blank line; and the requirement that the
`#` be followed by a space is relaxed _only_ for a comment on the first line of the document.

These two exceptions facilitate the inclusion of a shebang line at the start of a document, such as,
```codl
#!/usr/bin/env processor

model
  data
```

### Multiline values

Sometimes it is necessary to write a value containing more than one line of text, or which contains spaces,
or the character sequence ` #`, without being considered a comment. This is possible using a _double indent_:
instead of writing a key and its value on the same line, such as,
```codl
dog
  name        Fido
  description furry
```
we can write:
```codl
dog
  name Fido
  description
      Furry, brown and cuddly.
```

A double-indented value continues so long as its indentation level is maintained. Thus, in,
```codl
dog
  name Fido
  description
      Furry, brown
      and cuddly.
```
the value of `description` would be, `Furry, brown\nand cuddly`: the newline character (`\n`) is part of the
value, but the six spaces of indentation are not. Nevertheless, additional spaces may be included:
```codl
  description
      Furry, brown
       and cuddly
```
would be interpreted as a `Furry, brown\n and cuddly`, but any subsequent line with less indentation would
terminate the multiline value, and be interpreted as new data.

This is particularly useful for embedding other languages in CoDL. For example,
```codl
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

When embedding CoDL data in a host language, we often want to add additional indentation so that the
embedded CoDL aligns with the surrounding code. When a CoDL document is parsed, the indentation of the first
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

It is therefore also possible to interpret any contiguous fragment of a CoDL document provided no line
contains less indentation than the first line of data.

From the example at the top of the page, the fragment,
```codl
  module alpha
    name         Alpha
    description  This is a description
```
is itself a valid CoDL document.

## Binary form (BiCoDL)

CoDL can provide a convenient way of storing or transmitting tree-structured data. But for fast serialization
and deserialization, a binary form exists, as a direct translation of the same data model, which can be written
and read faster, and which uses less memory. This is called BiCoDL.

Although CoDL is a _binary_ format, in the sense that it is not primarily for human consumption, it contains only valid
printable UTF-8 characters, making it seamless to copy/paste or to embed within other textual data formats, such
as XML or JSON.

BCoDL should use the custom Media Type `application/x-bcodl`. BiCoDL data always begins with the
byte sequence, `b1` `c0` `d1`, which looks like `±ÀÑ` when interpreted as `UTF-8`.

## Schemas

A CoDL schema is an untyped CoDL document which may be used to verify other CoDL documents. Its structure should
mirror that of the documents it verifies: each line begins with, and defines, a keyword which may appear at that
position in the tree. The words that follow are the names of the parameters to that keyword.

Additionally, each keyword or paramater may be suffixed (without space) by a qualifying symbol.

### Qualifiers

- `?`: parameter or keyword is optional; may appear zero or once
- `+`: parameter or keyword is required and may appear once or many times
- `*`: parameter or keyword may appear zero, once or many times
- `!`: parameter is required and must be unique
- `&`: parameter is the concatenation of remaining words, including spaces and terminated by a newline
- `~`: keyword may also appear zero, once or many times in this section or any descendant

For example,
```codl-schema
child? arg
```
specifies the keyword `child` which may optionally appear once, with a required argument called `arg`. This schema
would validate the document,
```codl
child alpha
```

Or, for example,
```codl-schema
employee+ id! name&
```
specifies the `employee` keyword which must appear at least once, but may be repeated, with an `id` parameter that
must be unique, and a `name` parameter, which is the concatenation of the rest of the line. Such a schema would
verify, for example,
```codl
employee sgs Simon G. Smith
employee rp  Richard Price
```

### Verification

_Todo_
