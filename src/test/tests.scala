package cellulose

import probably.*
import gossamer.*
import rudiments.*
import turbulence.*
import eucalyptus.*
import tetromino.*
import parasitism.*, threading.platform

import java.io as ji

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"CoDL tests"):

  given Realm(t"tests")


  def run(using Runner): Unit = supervise(t"main"):
    import logging.silent
    import Token.*
    import Arity.*

    suite(t"Reader tests"):
      def interpret(text: Text)(using Log): PositionReader = PositionReader(ji.StringReader(text.s))

      test(t"Character can store line"):
        Character('©', 123, 456).line
      .assert(_ == 123)
      
      test(t"Character can store column"):
        Character('©', 123, 456).column
      .assert(_ == 456)
      
      test(t"Character can store Char"):
        Character('©', 123, 456).char
      .assert(_ == '©')
      
      test(t"Initial position is line 0"):
        val reader = interpret(t"abc")
        reader.next().line
      .assert(_ == 0)
      
      test(t"Initial position is column 0"):
        val reader = interpret(t"abc")
        reader.next().column
      .assert(_ == 0)
      
      test(t"Initial character is correct"):
        val reader = interpret(t"abc")
        reader.next().char
      .assert(_ == 'a')
      
      test(t"Initial linefeed character is correct"):
        val reader = interpret(t"\nabc")
        reader.next().char
      .assert(_ == '\n')
      
      test(t"Initial carriage return and linefeed gives linefeed"):
        val reader = interpret(t"\r\nabc")
        reader.next().char
      .assert(_ == '\n')
      
      test(t"Character following CR/LF is correct"):
        val reader = interpret(t"\r\nabc")
        reader.next()
        reader.next().char
      .assert(_ == 'a')
      
      test(t"Read a character gives column 1"):
        val reader = interpret(t"abc")
        reader.next()
        reader.next().column
      .assert(_ == 1)
      
      test(t"Read a character gives correct line"):
        val reader = interpret(t"abc")
        reader.next()
        reader.next().line
      .assert(_ == 0)
      
      test(t"Character after newline gives correct line"):
        val reader = interpret(t"\nabc")
        reader.next()
        reader.next().line
      .assert(_ == 1)
      
      test(t"Character after newline gives correct column"):
        val reader = interpret(t"\nabc")
        reader.next()
        reader.next().column
      .assert(_ == 0)
      
      test(t"Read a CR/LF character gives correct line"):
        val reader = interpret(t"\r\nabc")
        reader.next()
        reader.next().line
      .assert(_ == 1)
      
      test(t"character after CR/LF gives correct column"):
        val reader = interpret(t"\r\nabc")
        reader.next()
        reader.next().column
      .assert(_ == 0)
      
      test(t"after LF next newline does not fail"):
        val reader = interpret(t"a\nbc\n")
        for i <- 0 until 4 do reader.next()
        reader.next().char
      .assert(_ == '\n')
      
      test(t"after LF next newline must not include CR"):
        val reader = interpret(t"a\nbc\r\n")
        for i <- 0 until 4 do reader.next()
        capture(reader.next().char)
      .matches:
        case CodlParseError(_, _, CodlParseError.Issue.CarriageReturnMismatch(false)) =>
      
      test(t"after CR/LF next CR/LF does not fail"):
        val reader = interpret(t"a\r\nbc\r\n")
        for i <- 0 until 4 do reader.next()
        reader.next().char
      .assert(_ == '\n')
      
      test(t"after CR/LF next newline must include CR"):
        val reader = interpret(t"a\r\nbc\n")
        for i <- 0 until 4 do reader.next()
        capture(reader.next().char)
      .matches:
        case CodlParseError(_, _, CodlParseError.Issue.CarriageReturnMismatch(true)) =>
      
      test(t"can capture start of text"):
        val reader = interpret(t"abcdef")
        reader.put(reader.next())
        reader.put(reader.next())
        reader.put(reader.next())
        reader.get()
      .assert(_ == t"abc")
      
      test(t"capture is empty after get"):
        val reader = interpret(t"abcdef")
        for i <- 0 until 3 do reader.put(reader.next())
        reader.get()
        for i <- 0 until 3 do reader.put(reader.next())
        reader.get()
      .assert(_ == t"def")
      
      test(t"capture position is correct"):
        val reader = interpret(t"abcdef")
        for i <- 0 until 3 do reader.put(reader.next())
        reader.get()
        for i <- 0 until 3 do reader.put(reader.next())
        reader.get()
      .assert(_ == t"def")

      test(t"read end character"):
        val reader = interpret(t"")
        reader.next()
      .matches:
        case Character.End =>
      
      test(t"cannot read end twice"):
        val reader = interpret(t"")
        reader.next()
        capture(reader.next())
      .matches:
        case _: IllegalStateException =>

    suite(t"Tokenizer tests"):
      def parseText(text: Text)(using Log): (Int, LazyList[Token]) = Codl.tokenize(ji.StringReader(text.s))

      test(t"Parse two words with single space"):
        parseText(t"alpha beta")
      .assert(_ == (0, LazyList(Item(t"alpha", 0, 0), Item(t"beta", 0, 6))))
      

      test(t"Parse two words with trailing spaces"):
        parseText(t"alpha beta   ")
      .assert(_ == (0, LazyList(Item(t"alpha", 0, 0), Item(t"beta", 0, 6))))
      
      test(t"Parse two words with three spaces"):
        parseText(t"alpha   beta")
      .assert(_ == (0, LazyList(Item(t"alpha", 0, 0), Item(t"beta", 0, 8))))
      
      test(t"Parse two words with newline"):
        parseText(t"alpha beta\n")
      .assert(_ == (0, LazyList(Item(t"alpha", 0, 0), Item(t"beta", 0, 6))))

      test(t"Parse two words with two lines"):
        parseText(t"alpha\nbeta")
      .assert(_ == (0, LazyList(Item(t"alpha", 0, 0), Peer, Item(t"beta", 1, 0))))

      test(t"Parse two words on two lines with indentation"):
        parseText(t"alpha\n  beta")
      .assert(_ == (0, LazyList(Item(t"alpha", 0, 0), Indent, Item(t"beta", 1, 2))))

      test(t"Parse two words on two lines with initial indentation"):
        parseText(t" alpha\n   beta")
      .assert(_ == (1, LazyList(Item(t"alpha", 0, 1), Indent, Item(t"beta", 1, 3))))
      
      test(t"Parse two words on two lines with initial newline"):
        parseText(t"\nalpha\n  beta")
      .assert(_ == (0, LazyList(Item(t"alpha", 1, 0), Indent, Item(t"beta", 2, 2))))
      
      test(t"Parse text with whitespace on blank lines"):
        parseText(t"root\n  two\n\n \npeer")
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Indent, Item(t"two", 1, 2), Blank, Blank,
          Outdent(1), Item(t"peer", 4, 0))))
        
      test(t"Parse shebang"):
        parseText(t"""|#!/bin/bash
                      |root
                      |""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Comment(t"!/bin/bash", 0, 1), Peer, Item(t"root", 1, 0))))
      
      test(t"Parse initial comment"):
        parseText(t"""|# Initial comment
                      |root""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Comment(t" Initial comment", 0, 1), Peer, Item(t"root", 1, 0))))
      
      test(t"Parse two-line comment"):
        parseText(t"""|# Line 1
                      |# Line 2
                      |root""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Comment(t" Line 1", 0, 1), Peer, Comment(t" Line 2", 1, 1),
          Peer, Item(t"root", 2, 0))))
      
      test(t"Parse remark"):
        parseText(t"""|root # remark""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Comment(t"remark", 0, 7))))

      test(t"Parse non-remark"):
        parseText(t"""|root #not-a-remark""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Item(t"#not-a-remark", 0, 5))))

      test(t"Parse multi-word remark"):
        parseText(t"""|root # remark words""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Comment(t"remark words", 0, 7))))

      test(t"Parse double indentation"):
        parseText(t"""|root
                      |    child content
                      |""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Item(t"child content", 1, 4, true))))
      
      test(t"Parse double indentation then peer"):
        parseText(t"""|root
                      |    child content
                      |next
                      |""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Item(t"child content", 1, 4, true), Peer,
          Item(t"next", 2, 0))))
      
      test(t"Parse double indentation then peer with margin"):
        parseText(t"""| root
                      |     child content
                      | next
                      |""".s.stripMargin.show)
      .assert(_ == (1, LazyList(Item(t"root", 0, 1), Item(t"child content", 1, 5, true), Peer,
          Item(t"next", 2, 1))))
      
      test(t"Parse double indentation then peer with margin and indent"):
        parseText(t"""| root
                      |     child content
                      |   next
                      |""".s.stripMargin.show)
      .assert(_ == (1, LazyList(Item(t"root", 0, 1), Item(t"child content", 1, 5, true), Indent,
          Item(t"next", 2, 3))))
      
      test(t"Parse multiline content"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true))))
      
      test(t"Parse multiline content then peer"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |next
                      |""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true), Peer,
          Item(t"next", 3, 0))))
      
      test(t"Parse multiline content then indent"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |  next
                      |""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true), Indent,
          Item(t"next", 3, 2))))
      
      test(t"Parse multiline content then indented comment"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |  # comment
                      |""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true), Indent,
          Comment(t" comment", 3, 3))))
      
      test(t"Parse multiline content including a hash"):
        parseText(t"""|root
                      |    content
                      |    # not a comment
                      |""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Item(t"content\n# not a comment", 1, 4, true))))
      
      test(t"Parse multiline content including a additional indentation"):
        parseText(t"""|root
                      |    content
                      |     indented
                      |""".s.stripMargin.show)
      .assert(_ == (0, LazyList(Item(t"root", 0, 0), Item(t"content\n indented", 1, 4, true))))
      
      test(t"Surplus indentation"):
        capture:
          parseText(t"""|root
                        |     surplus-indented
                        |""".s.stripMargin.show)
      .assert(_ == CodlParseError(1, 5, CodlParseError.Issue.SurplusIndent))
      
      test(t"Uneven indentation"):
        capture:
          parseText(t"""|root
                        | uneven indented
                        |""".s.stripMargin.show)
      .assert(_ == CodlParseError(1, 1, CodlParseError.Issue.UnevenIndent(1, 0)))

      test(t"Uneven indentation 2"):
        capture:
          parseText(t"""|root
                        |   uneven indented
                        |""".s.stripMargin.show)
      .assert(_ == CodlParseError(1, 3, CodlParseError.Issue.UnevenIndent(3, 0)))
      
      test(t"Uneven de-indentation"):
        capture:
          parseText(t"""|root
                        |  child
                        | deindentation
                        |""".s.stripMargin.show)
      .assert(CodlParseError(1, 1, CodlParseError.Issue.SurplusIndent) == _)

    suite(t"Access tests"):
      val doc = Doc(
        Node(t"term")(
          Node(t"name")(Node(t"alpha")(), Node(t"beta")()),
          Node(t"name")(Node(t"gamma")()),
          Node(t"kind")(
            Node(t"query")(Node(t"value")())
          )
        ),
        Node(t"element")()
      )

      test(t"Access first element"):
        doc.term().key
      .assert(_ == t"term")

      test(t"Access second element"):
        doc.element().key
      .assert(_ == t"element")

      test(t"Access nested element"):
        doc.term().kind().key
      .assert(_ == t"kind")

      test(t"Access multiple keys"):
        doc.term().name.length
      .assert(_ == 2)
      
      test(t"Access deeper nested key"):
        doc.term(0).name(1)()
      .assert(_ == Data(t"gamma"))
      
      test(t"Access deeper nested param"):
        doc.term().name()(1)
      .assert(_ == Data(t"beta"))

    def read(text: Text)(using Log): Doc = Codl.parse(ji.StringReader(text.s))
    
    suite(t"Untyped parsing tests"):
      test(t"Empty document"):
        read(t"")
      .assert(_ == Doc())

      test(t"Simplest non-empty document"):
        read(t"root").wiped
      .assert(_ == Doc(Node(Data(t"root"))))
      
      test(t"Root peers"):
        read(t"root\nelement\n").wiped
      .assert(_ == Doc(Node(t"root")(), Node(t"element")()))

      test(t"Single child"):
        read(t"root\n  child").wiped
      .assert(_ == Doc(Node(t"root")(Node(t"child")())))
      
      test(t"Single child and peer"):
        read(t"root\n  child\npeer").wiped
      .assert(_ == Doc(Node(t"root")(Node(t"child")()), Node(t"peer")()))
      
      test(t"Single child and grandchild"):
        read(t"root\n  child\n    grandchild").wiped
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"grandchild")()))))
      
      test(t"Single child and grandchild and peer"):
        read(t"root\n  child\n    grandchild\npeer").wiped
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"grandchild")())), Node(t"peer")()))
      
      test(t"Peers at multiple levels"):
        read(t"root\n  child\n    grandchild\n  child\n    grandchild\npeer").wiped
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"grandchild")()),
          Node(t"child")(Node(t"grandchild")())), Node(t"peer")()))
    
      test(t"Data with parameter"):
        read(t"root param").wiped
      .assert(_ == Doc(Node(t"root")(Node(t"param")())))
      
      test(t"Data with remark"):
        read(t"root # remark").untyped
      .assert(_ == Doc(Node(Data(t"root"), Meta(0, Nil, t"remark"))))
      
      test(t"Data after comment"):
        read(t"# comment\nroot").untyped
      .assert(_ == Doc(Node(Data(t"root"), Meta(0, List(t" comment"), Unset))))
      
      test(t"Data after two comments"):
        read(t"# comment 1\n# comment 2\nroot").untyped
      .assert(_ == Doc(Node(Data(t"root"), Meta(0, List(t" comment 1", t" comment 2"), Unset))))
      
      test(t"Comment on child"):
        read(t"root\n  # comment\n  child").untyped
      .assert(_ == Doc(Node(Data(t"root", IArray(Node(Data(t"child"), Meta(0, List(t" comment"),
          Unset)))))))
      
      test(t"Comment and blank line on child"):
        read(t"root\n\n  # comment\n  child").untyped
      .assert(_ == Doc(Node(Data(t"root", IArray(Node(Data(t"child"), Meta(1, List(t" comment"),
          Unset)))))))
      
      test(t"Data with multiple parameters"):
        read(t"root param1 param2").wiped
      .assert(_ == Doc(Node(t"root")(Node(t"param1")(), Node(t"param2")())))
      
      test(t"Blank line before child"):
        read(t"root\n\n  child").untyped
      .assert(_ == Doc(Node(Data(t"root", IArray(Node(Data(t"child"), Meta(1, Nil)))))))
      
      test(t"Two blank lines before child"):
        read(t"root\n\n \n  child").untyped
      .assert(_ == Doc(Node(Data(t"root", IArray(Node(Data(t"child", IArray()), Meta(2, Nil, Unset)))))))
      
      test(t"Data with multiple parameters, remark and comment"):
        read(t"# comment\nroot param1 param2 # remark").untyped
      .assert(_ == Doc(Node(Data(t"root", IArray(Node(t"param1")(), Node(t"param2")())), Meta(0,
          List(t" comment"), t"remark"))))
      
      test(t"Data with multiple parameters, remark, comment and peer"):
        read(t"# comment\nroot param1 param2 # remark\npeer").untyped
      .assert(_ == Doc(Node(Data(t"root", IArray(Node(t"param1")(), Node(t"param2")())), Meta(0,
          List(t" comment"), t"remark")), Node(t"peer")()))
      
      test(t"Comment on blank node"):
        read(t"# comment\n\nroot").untyped
      .assert(_ == Doc(Node(Unset, Meta(0, List(t" comment"), Unset)), Node(Data(t"root"), Meta(1,
          Nil, Unset))))
      
      test(t"Remark after blank line"):
        read(t"root\n\npeer # remark").untyped
      .assert(_ == Doc(Node(t"root")(), Node(Data(t"peer"), Meta(0, Nil, t"remark"))))
      
      test(t"Long item"):
        read(t"root\n    one two\n").wiped
      .assert(_ == Doc(Node(t"root")(Node(t"one two")())))
      
      test(t"Multiline item"):
        read(t"root\n  child\n    this\n        one two\n        three four\n").wiped
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"this")(Node(t"one two\nthree four")())))))
      
    suite(t"Schema tests"):
      val childSchema = Struct(
                          t"one" -> Field(Optional),
                          t"two" -> Field(Optional)
                        )
      
      val grandchildSchema = Struct(t"data" -> Field(Optional))

      val childSchema2 = Struct(
                           t"alpha" -> grandchildSchema,
                           t"beta" -> Field(Optional)
                         )
                      
      val rootSchema = Struct(
                         t"child" -> childSchema,
                         t"second" -> childSchema2,
                         t"third" -> Field(Optional),
                         t"other" -> Struct(
                           t"param" -> Field(Optional),
                         )
                       )
      
      val topSchema = Struct(t"root" -> rootSchema)
      
      test(t"Root node has correct name"):
        topSchema.parse(t"root").untyped()
      .assert(_ == Data(t"root"))
      
      test(t"Root node has correct schema"):
        topSchema.parse(t"root")().schema
      .assert(_ == rootSchema)
      
      test(t"First child of root param is validated"):
        topSchema.parse(t"root\n  child").root()(0).schema
      .assert(_ == childSchema)
      
      test(t"Third child of root param is validated"):
        val doc = topSchema.parse(t"root\n  child\n  second\n  third value\n")
        doc.root()(2).schema
      .assert(_ == Field(Optional))
      
      test(t"Second child of root param is validated"):
        val doc = topSchema.parse(t"root\n  child\n  second")
        doc.root()(1).schema
      .assert(_ == childSchema2)
      
      test(t"Child is validated"):
        topSchema.parse(t"root\n  child").root().child().schema
      .assert(_ == childSchema)
      
      test(t"Different-named child is validated"):
        topSchema.parse(t"root\n  second").root().second().schema
      .assert(_ == childSchema2)
      
      test(t"Grandchild nodes are validated"):
        topSchema.parse(t"root\n  second\n    alpha").root().second().alpha().schema
      .assert(_ == grandchildSchema)
      
      test(t"Invalid top-level node"):
        capture(topSchema.parse(t"riot"))
      .assert(_ == CodlValidationError(t"riot", InvalidKey(t"riot")))
      
      test(t"Validate second top-level node"):
        rootSchema.parse(t"child\nsecond").second().schema
      .assert(_ == childSchema2)
      
      test(t"Validate second top-level node out of order"):
        rootSchema.parse(t"second\nchild").second().schema
      .assert(_ == childSchema2)
      
      test(t"Validate second top-level node after child"):
        rootSchema.parse(t"child\n  one\nsecond").second().schema
      .assert(_ == childSchema2)
      
      val requiredChild = Struct(
                            t"root" -> Struct(
                              t"child" -> Field(One)
                            )
                          )
      
      test(t"Missing required node throws exception"):
        capture(requiredChild.parse(t"root"))
      .assert(_ == CodlValidationError(t"root", MissingKey(t"child")))
      
      test(t"Present required node does not throw exception"):
        requiredChild.parse(t"root\n  child").untyped.root().child()
      .assert(_ == Data(t"child"))
      
      val repeatableChild = Struct(
                              t"root" -> Struct(
                                t"child" -> Field(Many)
                              )
                            )
      
      test(t"Duplicated unique child is forbidden"):
        capture(requiredChild.parse(t"root\n  child\n  child"))
      .assert(_ == CodlValidationError(t"child", DuplicateKey(t"child")))
      
      test(t"Duplicated repeatable child is permitted"):
        repeatableChild.parse(t"root\n  child\n  child").untyped.root().child(1)
      .assert(_ == Data(t"child"))
      
      val atLeastOneChild = Struct(
                              t"root" -> Struct(
                                t"child" -> Field(AtLeastOne)
                              )
                            )
      
      test(t"'At least one' may mean one"):
        atLeastOneChild.parse(t"root\n  child").untyped.root().child()
      .assert(_ == Data(t"child"))
      
      test(t"'At least one' may mean two"):
        atLeastOneChild.parse(t"root\n  child\n  child").untyped.root().child(1)
      .assert(_ == Data(t"child"))
      
      test(t"'At least one' may not mean zero"):
        capture(requiredChild.parse(t"root"))
      .assert(_ == CodlValidationError(t"root", MissingKey(t"child")))
      
      def childWithTwoParams(alpha: Arity, beta: Arity) =
        Struct(
          t"root" -> Struct(
            t"child" -> Struct(
              t"alpha" -> Field(alpha),
              t"beta" -> Field(beta)
            )
          )
        )
      
      def rootWithTwoParams(alpha: Arity, beta: Arity) =
        Struct(
          t"child" -> Struct(
            t"alpha" -> Field(alpha),
           t"beta" -> Field(beta)
          )
        )
      
      test(t"Access parameters by name"):
        childWithTwoParams(One, One).parse(t"root\n  child first second").root().child().beta()
      .assert(_ == Data(t"second", IArray(), schema = Field(One)))

      test(t"Surplus parameters"):
        capture(childWithTwoParams(One, One).parse(t"root\n  child one two three"))
      .assert(_ == CodlValidationError(t"three", SurplusParams(t"child")))
      
      test(t"Two surplus parameters"):
        capture(childWithTwoParams(One, One).parse(t"root\n  child one two three four"))
      .assert(_ == CodlValidationError(t"three", SurplusParams(t"child")))
      
      test(t"Two optional parameters not specified"):
        childWithTwoParams(Optional, Optional).parse(t"root\n  child").root().child().paramCount
      .assert(_ == 0)
      
      test(t"Two optional parameters with one specified"):
        childWithTwoParams(Optional, Optional).parse(t"root\n  child one").root().child().paramCount
      .assert(_ == 1)
      
      test(t"Two optional parameters with both specified"):
        childWithTwoParams(Optional, Optional).parse(t"root\n  child one two").root().child().paramCount
      .assert(_ == 2)
      
      test(t"Two optional parameters with one surplus"):
        capture(childWithTwoParams(Optional, Optional).parse(t"root\n  child one two three").root().child())
      .assert(_ == CodlValidationError(t"three", SurplusParams(t"child")))
      
      test(t"Variadic parameters are counted"):
        childWithTwoParams(One, Many).parse(t"root\n  child one two three four").root().child().paramCount
      .assert(_ == 4)
      
      test(t"Variadic parameter has right number of values"):
        childWithTwoParams(One, Many).parse(t"root\n  child one two three four").root().child().beta.length
      .assert(_ == 3)
      
      test(t"Variadic parameters are optional"):
        childWithTwoParams(One, Many).parse(t"root\n  child one").root().child().paramCount
      .assert(_ == 1)
      
      test(t"'at least one' parameters are not optional"):
        capture(childWithTwoParams(One, AtLeastOne).parse(t"root\n  child one"))
      .assert(_ == CodlValidationError(t"child", MissingKey(t"beta")))
      
      test(t"Variadic first parameters don't count for second"):
        capture(childWithTwoParams(AtLeastOne, AtLeastOne).parse(t"root\n  child one two three"))
      .assert(_ == CodlValidationError(t"child", MissingKey(t"beta")))
      
      test(t"Params and children can be mixed"):
        capture(childWithTwoParams(AtLeastOne, AtLeastOne).parse(t"root\n  child one two three\n    beta one"))
      .assert(_ == CodlValidationError(t"child", MissingKey(t"beta")))

      test(t"Two optional parameters not specified on root"):
        rootWithTwoParams(Optional, Optional).parse(t"  child").child().paramCount
      .assert(_ == 0)
      
      test(t"Two optional parameters with one specified on root"):
        rootWithTwoParams(Optional, Optional).parse(t"  child one").child().paramCount
      .assert(_ == 1)
      
      test(t"Two optional parameters with both specified on root"):
        rootWithTwoParams(Optional, Optional).parse(t"  child one two").child().paramCount
      .assert(_ == 2)
      
      test(t"Two optional parameters with one surplus on root"):
        capture(rootWithTwoParams(Optional, Optional).parse(t"  child one two three").child())
      .assert(_ == CodlValidationError(t"three", SurplusParams(t"child")))
      
      test(t"Variadic parameters are counted on root"):
        rootWithTwoParams(One, Many).parse(t"  child one two three four").child().paramCount
      .assert(_ == 4)
      
      test(t"Variadic parameter has right number of values on root"):
        rootWithTwoParams(One, Many).parse(t"  child one two three four").child().beta.length
      .assert(_ == 3)
      
      test(t"Variadic parameters are optional on root"):
        rootWithTwoParams(One, Many).parse(t"  child one").child().paramCount
      .assert(_ == 1)
      
      test(t"'at least one' parameters are not optional on root"):
        capture(rootWithTwoParams(One, AtLeastOne).parse(t"  child one"))
      .assert(_ == CodlValidationError(t"child", MissingKey(t"beta")))
      
      test(t"Variadic first parameters don't count for second on root"):
        capture(rootWithTwoParams(AtLeastOne, AtLeastOne).parse(t"  child one two three"))
      .assert(_ == CodlValidationError(t"child", MissingKey(t"beta")))
      
      test(t"Params and children can be mixed on root"):
        capture(rootWithTwoParams(AtLeastOne, AtLeastOne).parse(t"  child one two three\n    beta one"))
      .assert(_ == CodlValidationError(t"child", MissingKey(t"beta")))

    suite(t"Binary tests"):

      val schema = Struct(
        t"field" -> Struct(
          t"child" -> Field(Optional)
        ),
        t"field2" -> Field(Optional),
        t"field3" -> Field(Optional)
      )
      
      def roundtrip(doc: Doc)(using Log): Doc = Bin.read(doc.schema, ji.StringReader(doc.binary.s).nn)

      val doc = schema.parse(t"field")
      
      test(t"Serialize and deserialize one node"):
        roundtrip(doc)
      .assert(_ == doc.uncommented)

      val doc2 = schema.parse(t"field\n  child\n")
      test(t"Serialize and deserialize one node and child"):
        roundtrip(doc2)
      .assert(_ == doc2.uncommented)

      val doc3 = schema.parse(t"field\n  child value")
      test(t"Serialize and deserialize one node and child/param"):
        roundtrip(doc3)
      .assert(_ == doc3.uncommented)
      
      val doc4 = schema.parse(t"field\n  child value value2 value3")
      test(t"Serialize and deserialize one node and child with three params"):
        roundtrip(doc4)
      .assert(_ == doc4.uncommented)

      val doc5 = schema.parse(t"field\n  child value value2\nfield2\nfield3")
      test(t"Serialize and deserialize document with indent, outdent and peer"):
        roundtrip(doc5)
      .assert(_ == doc5.uncommented)

    
    def roundtrip[T: Codec](value: T): T = value.codl.as[T]

    suite(t"Generic Derivation tests"):

      case class Person(name: Text, age: Int)
      case class Organisation(name: Text, ceo: Person)
      case class Player(name: Text, rank: Maybe[Int])

      test(t"write a simple case class"):
        Person(t"John Smith", 65).codl.untyped
      .assert(_ == Doc(Node(t"name")(Node(t"John Smith")()), Node(t"age")(Node(t"65")())))
      
      test(t"write a nested case class"):
        val person1 = Person(t"Alpha", 1)
        Organisation(t"Acme", person1).codl.untyped
      .assert(_ == Doc(Node(t"name")(Node(t"Acme")()), Node(t"ceo")(Node(t"name")(Node(t"Alpha")()),
          Node(t"age")(Node(t"1")()))))
      
      test(t"write a case class with optional field specified"):
        Player(t"Barry", 1).codl.untyped
      .assert(_ == Doc(Node(t"name")(Node(t"Barry")()), Node(t"rank")(Node(t"1")())))
      
      test(t"write a case class with optional field unspecified"):
        Player(t"Barry", Unset).codl.untyped
      .assert(_ == Doc(Node(t"name")(Node(t"Barry")())))
      
      test(t"serialize a List of case classes"):
        List(Person(t"John Smith", 65), Person(t"Jim Calvin", 11)).codl.untyped
      .assert(_ == Doc(
        Node(t"item")(Node(t"name")(Node(t"John Smith")()), Node(t"age")(Node(t"65")())),
        Node(t"item")(Node(t"name")(Node(t"Jim Calvin")()), Node(t"age")(Node(t"11")()))
      ))
      
      test(t"serialize a List of integers"):
        List(1, 2, 3).codl.untyped
      .assert(_ == Doc(Node(t"1")(), Node(t"2")(), Node(t"3")()))
      
      test(t"serialize a List of booleans"):
        List(true, false, true).codl.untyped
      .assert(_ == Doc(Node(t"yes")(), Node(t"no")(), Node(t"yes")()))

      test(t"roundtrip a true boolean"):
        roundtrip[Boolean](true)
      .assert(_ == true)
      
      test(t"roundtrip a false boolean"):
        roundtrip[Boolean](false)
      .assert(_ == false)
      
      test(t"roundtrip an integer"):
        roundtrip[Int](42)
      .assert(_ == 42)
      
      test(t"roundtrip some text"):
        roundtrip[Text](t"hello world")
      .assert(_ == t"hello world")
      
      test(t"roundtrip a list of strings"):
        roundtrip[List[Text]](List(t"hello", t"world"))
      .assert(_ == List(t"hello", t"world"))
      
      test(t"roundtrip a list of case classes"):
        roundtrip[List[Person]](List(Person(t"Jack", 12), Person(t"Bill", 32)))
      .assert(_ == List(Person(t"Jack", 12), Person(t"Bill", 32)))
      
      case class Foo(alpha: Text, beta: Maybe[Text])
      test(t"roundtrip a case class with an optional parameter"):
        roundtrip(Foo(t"one", t"two"))
      .assert(_ == Foo(t"one", t"two"))
      
      test(t"roundtrip a case class with an optional parameter, unset"):
        roundtrip(Foo(t"one", Unset))
      .assert(_ == Foo(t"one", Unset))

      case class Bar(foo: List[Baz], quux: Quux)
      case class Quux(alpha: Text, beta: List[Byte])
      case class Baz(gamma: Text, delta: Int, eta: Maybe[Char])
      
      val complex = Bar(List(Baz(t"a", 2, Unset), Baz(t"c", 6, 'e')), Quux(t"e", List(1, 2, 4)))
      
      test(t"roundtrip a complex case class"):
        roundtrip(complex)
      .assert(_ == complex)

      def print[T: Codec](value: T): Text =
        val writer = new ji.StringWriter()
        Printer.print(writer, value.codl)
        writer.toString().show

      test(t"print a simple case class"):
        print(Foo(t"one", t"two"))
      .assert(_ == t"alpha one\nbeta two\n")
      
      test(t"print a complex case class"):
        print(complex)
      .assert(_ == t"foo\n  item\n    gamma a\n    delta 2\n  item\n    gamma c\n    delta 6\n    eta e\nquux\n  alpha e\n  beta 1 2 4\n")
      
      test(t"roundtrip a complex case class "):
        read(print(complex)).as[Bar]
      .assert(_ == complex)
      
    suite(t"Record tests"):

      val record = GreekRecords(example1)
      
      test[Maybe[Text]](t"Test optional return value"):
        record.alpha
      .assert(_ == t"one")
      
      test(t"Test return value"):
        record.beta
      .assert(_ == t"two")
      
      test[Maybe[Text]](t"Test missing value"):
        record.iota
      .assert(_ == Unset)
      
      test[Maybe[Text]](t"Test unique return value"):
        record.eta
      .assert(_ == t"eight")
      
      test(t"Test many return value"):
        record.gamma
      .assert(_ == List(t"three", t"four", t"five"))
      
      test(t"Test multiple return value"):
        record.kappa
      .assert(_ == List(t"nine", t"ten", t"eleven"))
    //   test(t"write a nested case class"):
    //     Organisation(t"Acme Inc", Person(t"John Smith", 65)).codl.show
    //   .assert(_ == t"""|name
    //                    |    Acme Inc
    //                    |ceo
    //                    |  name
    //                    |      John Smith
    //                    |  age 65
    //                    |""".s.stripMargin.show)

    //   test(t"serialize a simple case class"):
    //     Person(t"John", 65).codl.show
    //   .assert(_ == t"""|name John
    //                    |age 65
    //                    |""".s.stripMargin.show)
      
    //   test(t"serialize a case class with long string"):
    //     Person(t"John Smith", 65).codl.show
    //   .assert(_ == t"""|name
    //                    |    John Smith
    //                    |age 65
    //                    |""".s.stripMargin.show)
      
    //   test(t"serialize a nested case class"):
    //     Organisation(t"Acme", Person(t"John", 65)).codl.show
    //   .assert(_ == t"""|name Acme
    //                    |ceo John 65
    //                    |""".s.stripMargin.show)

    //   test(t"serialize a nested case class and long strings"):
    //     Organisation(t"Acme Inc", Person(t"John Smith", 65)).codl.show
    //   .assert(_ == t"""|name
    //                    |    Acme Inc
    //                    |ceo
    //                    |  name
    //                    |      John Smith
    //                    |  age 65
    //                    |""".s.stripMargin.show)
      
    //   test(t"serialize a list of items"):
    //     Items(Item(t"one"), Item(t"two"), Item(t"three")).codl.show
    //   .assert(_ == t"""|item one
    //                    |item two
    //                    |item three
    //                    |""".s.stripMargin.show)
      
    //   test(t"serialize a list of case classes"):
    //     Items(Person(t"Tom", 10), Person(t"Dick", 11), Person(t"Harry", 12)).codl.show
    //   .assert(_ == t"""|item Tom 10
    //                    |item Dick 11
    //                    |item Harry 12
    //                    |""".s.stripMargin.show)
      
    //   test(t"serialize a list of case classes with long names"):
    //     Items(Person(t"Tom Smith", 10), Person(t"Dick Smith", 11), Person(t"Harry Smith", 12)).codl.show
    //   .assert(_ == t"""|item
    //                    |  name
    //                    |      Tom Smith
    //                    |  age 10
    //                    |item
    //                    |  name
    //                    |      Dick Smith
    //                    |  age 11
    //                    |item
    //                    |  name
    //                    |      Harry Smith
    //                    |  age 12
    //                    |""".s.stripMargin.show)
      
    //   test(t"serialize a list of param/children case classes"):
    //     Items(Organisation(t"Acme", Person(t"Tom Smith", 10)), Organisation(t"Alpha One",
    //         Person(t"Harry", 11))).codl.show
    //   .assert(_ == t"""|item Acme
    //                    |  ceo
    //                    |    name
    //                    |        Tom Smith
    //                    |    age 10
    //                    |item
    //                    |  name
    //                    |      Alpha One
    //                    |  ceo Harry 11
    //                    |""".s.stripMargin.show)
      
    //   test(t"serialize a mixture of two lists"):
    //     Meal(List(Drink(t"Coke"), Drink(t"Lemonade")), List(Food(t"Apple"), Food(t"Orange"))).codl.show
    //   .assert(_ == t"""|drink Coke
    //                    |drink Lemonade
    //                    |food Apple
    //                    |food Orange
    //                    |""".s.stripMargin.show)

    // suite(t"Data roundtrip tests"):
    //   def roundtrip[T: SchemaGen: Serializer: Deserializer](value: T)(using Log): T =
    //     val text = value.codl
    //     Log.info(text.toString)
    //     Log.info(text.show)
    //     Codl.parse(text.show).as[T]

    //   case class Foo(item: Text)
    //   case class Bar(foo1: Foo, foo2: Foo)
    //   case class Baz(bar: Bar, foo: Foo)
      
    //   test(t"deserialize simplest case class"):
    //     roundtrip(Foo(t"abc"))
    //   .assert(_ == Foo(t"abc"))
      
    //   test(t"deserialize more complex case class"):
    //     roundtrip(Bar(Foo(t"abc"), Foo(t"xyz")))
    //   .assert(_ == Bar(Foo(t"abc"), Foo(t"xyz")))
      
    //   test(t"deserialize doubly-nested case class"):
    //     roundtrip(Baz(Bar(Foo(t"abc"), Foo(t"xyz")), Foo(t"test")))
    //   .assert(_ == Baz(Bar(Foo(t"abc"), Foo(t"xyz")), Foo(t"test")))
      
    //   test(t"deserialize doubly-nested case class with multiline strings"):
    //     roundtrip(Baz(Bar(Foo(t"abc"), Foo(t"xyz")), Foo(t"abc\ndef")))
    //   .assert(_ == Baz(Bar(Foo(t"abc"), Foo(t"xyz")), Foo(t"abc\ndef")))
      
    //   test(t"deserialize doubly-nested case class with several long and multiline strings"):
    //     roundtrip(Baz(Bar(Foo(t"abc 123"), Foo(t"xyz\n456 789")), Foo(t"abc\ndef")))
    //   .assert(_ == Baz(Bar(Foo(t"abc 123"), Foo(t"xyz\n456 789")), Foo(t"abc\ndef")))
      
    //   test(t"automatically trim strings with extra space"):
    //     roundtrip(Baz(Bar(Foo(t" abc 123   "), Foo(t"  xyz\n456 789  ")), Foo(t"   abc\ndef ")))
    //   .assert(_ == Baz(Bar(Foo(t"abc 123"), Foo(t"xyz\n456 789")), Foo(t"abc\ndef")))
      
    //   test(t"serialize and deserialize a case class"):
    //     val codl = Person(t"John Smith", 65).codl
    //     Codl.parse(codl.show).as[Person]
    //   .assert(_ == Person(t"John Smith", 65))
      
    //   test(t"roundtrip a nested case class with a long strings"):
    //     val org = Organisation(t"Acme Inc", Person(t"John Smith", 65))
    //     Codl.parse(org.codl.show).as[Organisation]
    //   .assert(_ == Organisation(t"Acme Inc", Person(t"John Smith", 65)))
      
    //   test(t"roundtrip a mixture of two lists"):
    //     roundtrip(Meal(List(Drink(t"Coke"), Drink(t"Lemonade")), List(Food(t"Apple"), Food(t"Orange"))))
    //   .assert(_ == Meal(List(Drink(t"Coke"), Drink(t"Lemonade")), List(Food(t"Apple"), Food(t"Orange"))))
      
    //   test(t"roundtrip a list of param/children case classes"):
    //     roundtrip(Items(Organisation(t"Acme", Person(t"Tom Smith", 10)), Organisation(t"Alpha One",
    //         Person(t"Harry", 11))))
    //   .assert(_ == Items(Organisation(t"Acme", Person(t"Tom Smith", 10)), Organisation(t"Alpha One",
    //       Person(t"Harry", 11))))
      