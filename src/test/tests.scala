package codl

import probably.*
import gossamer.*
import rudiments.*
import turbulence.*
import eucalyptus.*
import tetromino.*
import parasitism.*

import java.io.StringReader

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"CoDL tests"):

  given Realm(t"tests")


  def run(using Runner): Unit = supervise(t"main"):
    import logging.silent
    import Token.*

    suite(t"Reader tests"):
      def interpret(text: Text)(using Log): PositionReader = PositionReader(StringReader(text.s))

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
      def parseText(text: Text)(using Log): CodlStream = Codl.tokenize(StringReader(text.s))

      test(t"Parse two words with single space"):
        parseText(t"alpha beta")
      .assert(_ == CodlStream(0, LazyList(Item(t"alpha", 0, 0), Item(t"beta", 0, 6))))
      

      test(t"Parse two words with trailing spaces"):
        parseText(t"alpha beta   ")
      .assert(_ == CodlStream(0, LazyList(Item(t"alpha", 0, 0), Item(t"beta", 0, 6))))
      
      test(t"Parse two words with three spaces"):
        parseText(t"alpha   beta")
      .assert(_ == CodlStream(0, LazyList(Item(t"alpha", 0, 0), Item(t"beta", 0, 8))))
      
      test(t"Parse two words with newline"):
        parseText(t"alpha beta\n")
      .assert(_ == CodlStream(0, LazyList(Item(t"alpha", 0, 0), Item(t"beta", 0, 6))))

      test(t"Parse two words with two lines"):
        parseText(t"alpha\nbeta")
      .assert(_ == CodlStream(0, LazyList(Item(t"alpha", 0, 0), Peer, Item(t"beta", 1, 0))))

      test(t"Parse two words on two lines with indentation"):
        parseText(t"alpha\n  beta")
      .assert(_ == CodlStream(0, LazyList(Item(t"alpha", 0, 0), Indent, Item(t"beta", 1, 2))))

      test(t"Parse two words on two lines with initial indentation"):
        parseText(t" alpha\n   beta")
      .assert(_ == CodlStream(1, LazyList(Item(t"alpha", 0, 1), Indent, Item(t"beta", 1, 3))))
      
      test(t"Parse two words on two lines with initial newline"):
        parseText(t"\nalpha\n  beta")
      .assert(_ == CodlStream(0, LazyList(Item(t"alpha", 1, 0), Indent, Item(t"beta", 2, 2))))
      
      test(t"Parse text with whitespace on blank lines"):
        parseText(t"root\n  two\n\n \npeer")
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Indent, Item(t"two", 1, 2), Blank, Blank,
          Outdent(1), Item(t"peer", 4, 0))))
        
      test(t"Parse shebang"):
        parseText(t"""|#!/bin/bash
                      |root
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Comment(t"!/bin/bash", 0, 1), Peer, Item(t"root", 1, 0))))
      
      test(t"Parse initial comment"):
        parseText(t"""|# Initial comment
                      |root""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Comment(t" Initial comment", 0, 1), Peer, Item(t"root", 1, 0))))
      
      test(t"Parse two-line comment"):
        parseText(t"""|# Line 1
                      |# Line 2
                      |root""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Comment(t" Line 1", 0, 1), Peer, Comment(t" Line 2", 1, 1),
          Peer, Item(t"root", 2, 0))))
      
      test(t"Parse remark"):
        parseText(t"""|root # remark""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Comment(t"remark", 0, 7))))

      test(t"Parse non-remark"):
        parseText(t"""|root #not-a-remark""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Item(t"#not-a-remark", 0, 5))))

      test(t"Parse multi-word remark"):
        parseText(t"""|root # remark words""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Comment(t"remark words", 0, 7))))

      test(t"Parse double indentation"):
        parseText(t"""|root
                      |    child content
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Item(t"child content", 1, 4, true))))
      
      test(t"Parse double indentation then peer"):
        parseText(t"""|root
                      |    child content
                      |next
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Item(t"child content", 1, 4, true), Peer, Item(t"next", 2, 0))))
      
      test(t"Parse double indentation then peer with margin"):
        parseText(t"""| root
                      |     child content
                      | next
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(1, LazyList(Item(t"root", 0, 1), Item(t"child content", 1, 5, true), Peer, Item(t"next", 2, 1))))
      
      test(t"Parse double indentation then peer with margin and indent"):
        parseText(t"""| root
                      |     child content
                      |   next
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(1, LazyList(Item(t"root", 0, 1), Item(t"child content", 1, 5, true), Indent, Item(t"next", 2, 3))))
      
      test(t"Parse multiline content"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true))))
      
      test(t"Parse multiline content then peer"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |next
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true), Peer, Item(t"next", 3, 0))))
      
      test(t"Parse multiline content then indent"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |  next
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true), Indent, Item(t"next", 3, 2))))
      
      test(t"Parse multiline content then indented comment"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |  # comment
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true), Indent, Comment(t" comment", 3, 3))))
      
      test(t"Parse multiline content including a hash"):
        parseText(t"""|root
                      |    content
                      |    # not a comment
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Item(t"content\n# not a comment", 1, 4, true))))
      
      test(t"Parse multiline content including a additional indentation"):
        parseText(t"""|root
                      |    content
                      |     indented
                      |""".s.stripMargin.show)
      .assert(_ == CodlStream(0, LazyList(Item(t"root", 0, 0), Item(t"content\n indented", 1, 4, true))))
      
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
        doc.term().name(1).children
      .assert(_ == List(Node(t"gamma")()))
      
      test(t"Access deeper nested param"):
        doc.term().name(0).beta().key
      .assert(_ == t"beta")

    def read(text: Text)(using Log): Doc = Codl.build(Codl.tokenize(StringReader(text.s)))
    
    suite(t"Untyped parsing tests"):
      // test(t"Empty document"):
      //   read(t"")
      // .assert(_ == Doc())
      
      test(t"Simplest non-empty document"):
        read(t"root")
      .assert(_ == Doc(Node(Data(t"root"))))
      
      test(t"Root peers"):
        read(t"root\nelement\n")
      .assert(_ == Doc(Node(t"root")(), Node(t"element")()))

      test(t"Single child"):
        read(t"root\n  child")
      .assert(_ == Doc(Node(t"root")(Node(t"child")())))
      
      test(t"Single child and peer"):
        import logging.stdout
        read(t"root\n  child\npeer")
      .assert(_ == Doc(Node(t"root")(Node(t"child")()), Node(t"peer")()))
      
      test(t"Single child and grandchild"):
        read(t"root\n  child\n    grandchild")
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"grandchild")()))))
      
      test(t"Single child and grandchild and peer"):
        read(t"root\n  child\n    grandchild\npeer")
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"grandchild")())), Node(t"peer")()))
      
      test(t"Peers at multiple levels"):
        read(t"root\n  child\n    grandchild\n  child\n    grandchild\npeer")
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"grandchild")()), Node(t"child")(Node(t"grandchild")())), Node(t"peer")()))
    
      test(t"Data with parameter"):
        read(t"root param")
      .assert(_ == Doc(Node(t"root")(Node(t"param")())))
      
      test(t"Data with remark"):
        read(t"root # remark")
      .assert(_ == Doc(Node(Data(t"root", Nil, Unset), Meta(0, Nil, t"remark"))))
      
      test(t"Data after comment"):
        read(t"# comment\nroot")
      .assert(_ == Doc(Node(Data(t"root", Nil, Unset), Meta(0, List(t"remark"), Unset))))
      
      test(t"Data after two comments"):
        read(t"# comment 1\n# comment 2\nroot")
      .assert(_ == Doc(Node(Data(t"root", Nil, Unset), Meta(0, List(t" comment 1", t" comment 2"), Unset))))
      
      test(t"Data with multiple parameters"):
        read(t"root param1 param2")
      .assert(_ == Doc(Node(t"root")(Node(t"param1")(), Node(t"param2")())))
      
    //   val schema = Schema(ListMap(
    //                  t"root" -> Schema(ListMap(
    //                    t"first" -> Schema(),
    //                    t"second" -> Schema(),
    //                    t"third" -> Schema(),
    //                    t"child" -> Schema(ListMap(
    //                      t"one" -> Schema(),
    //                      t"two" -> Schema()
    //                    )),
    //                    t"other" -> Schema(ListMap(
    //                      t"param" -> Schema(ListMap(), Multiplicity.Joined),
    //                    ))
    //                  ))
    //                ))

    //   test(t"Root and param"):
    //     schema.parse(t"root param")
    //   .assert(_ == Doc(Node(t"root")(Node(t"first")(Node(t"param")()))))
      
    //   test(t"Root and two params"):
    //     schema.parse(t"root param1 param2")
    //   .assert(_ == Doc(Node(t"root")(Node(t"first")(Node(t"param1")()), Node(t"second")(Node(t"param2")()))))
      
    //   test(t"Root and three params"):
    //     schema.parse(t"root param1 param2 param3")
    //   .assert(_ == Doc(Node(t"root")(Node(t"first")(Node(t"param1")()), Node(t"second")(Node(t"param2")()), Node(t"third")(Node(t"param3")()))))
      
    //   test(t"Child with param"):
    //     schema.parse(t"root\n  child param3")
    //   .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"one")(Node(t"param3")())))))
      
    //   test(t"Root with indent of 1"):
    //     Schema.Freeform.parse(t" root")
    //   .assert(_ == Doc(List(Node(t"root")()), 1))

    //   test(t"Single child with indent of 3"):
    //     Schema.Freeform.parse(t"   root\n     child")
    //   .assert(_ == Doc(List(Node(t"root")(Node(t"child")())), 3))
      
    //   test(t"Single child with joined parameter"):
    //     schema.parse(t"root\n  other Hello World")
    //   .assert(_ == Doc(Node(t"root")(Node(t"other")(Node(t"param")(Node(t"Hello World")())))))

    //   test(t"Parse peer root nodes (with indent)"):
    //     Schema.Freeform.parse(t"  root\n  peer")
    //   .assert(_ == Doc(List(Node(t"root")(), Node(t"peer")()), 2))
      
    //   test(t"Parse peer root node with parameters"):
    //     Schema.Freeform.parse(t"  root  param1 param2\n  peer")
    //   .assert(_ == Doc(List(Node(t"root")(Node(t"param1")(), Node(t"param2")()), Node(t"peer")()), 2))

    // suite(t"Parsing tests"):
    //   import Line.*
    //   test(t"Parse smallest document"):
    //     Codl.parse(t"""root""".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root")))
      
    //   test(t"Parse top-level peers"):
    //     Codl.parse(t"root\nnext".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root"), Node(t"next")))
      
    //   test(t"Parse root and one child"):
    //     Codl.parse(t"root\n  child".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(), IArray(Node(t"child")))))
      
    //   test(t"Parse root and two children"):
    //     Codl.parse(t"root\n  child1\n  child2".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(), IArray(Node(t"child1"), Node(t"child2")))))
      
    //   test(t"Parse root and two children with outdent"):
    //     Codl.parse(t"root\n  child1\n  child2\ntoplevel".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(), IArray(Node(t"child1"), Node(t"child2"))),
    //       Node(t"toplevel")))

    //   test(t"Parse root with double outdent"):
    //     Codl.parse(t"one\n  child\n    grandchild\ntwo".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"one", Map(), IArray(Node(t"child", Map(),
    //       IArray(Node(t"grandchild"))))), Node(t"two")))
      
    //   test(t"Single node with comment"):
    //     Codl.parse(t"#xyz\none".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"one", Map(), IArray(), Meta(List(t"xyz"), 0, None))))
      
    //   test(t"Single node with comment and blank line in between"):
    //     Codl.parse(t"#xyz\n\none".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Gap(List(t"xyz"), 1), Node(t"one", Map(), IArray())))
      
    //   test(t"Single node with 'long' one-word param"):
    //     Codl.parse(t"root\n    param".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"param"))))
      
    //   test(t"Single node with long multi-word param"):
    //     Codl.parse(t"root\n    The quick brown fox".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"The quick brown fox"))))
      
    //   test(t"Single node with long multi-line param"):
    //     Codl.parse(t"root\n    The quick brown fox\n    jumps over the lazy dog.".s.stripMargin.show).children
    //         .to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode ->
    //       t"The quick brown fox\njumps over the lazy dog."))))
      
    //   test(t"Single node with multiline-param and indentation"):
    //     Codl.parse(t"root\n    The quick brown fox\n     jumps over the lazy dog.".s.stripMargin.show).children
    //         .to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode ->
    //       t"The quick brown fox\n jumps over the lazy dog."))))
      
    //   test(t"Single node with multiline-param then child"):
    //     Codl.parse(t"root\n    two\n    lines\n  child".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"two\nlines"),
    //       IArray(Node(t"child")))))
    
    //   test(t"Single node with multiline-param then peer"):
    //     Codl.parse(t"root\n    two\n    lines\npeer".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"two\nlines")), Node(t"peer")))
      
    //   test(t"Single node with multiline-param then blank line, then peer"):
    //     Codl.parse(t"root\n    two\n    lines\n\npeer".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"two\nlines")), Gap(List(), 1),
    //       Node(t"peer")))
      
    //   test(t"Single node with multiline-param then two blank lines, then peer"):
    //     Codl.parse(t"root\n    two\n    lines\n\n\npeer".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"two\nlines")),
    //       Gap(List(), 2), Node(t"peer")))
      
    //   test(t"Single node with multiline-param then two blank lines with surplus whitespace, then peer"):
    //     Codl.parse(t"root\n    two\n    lines\n\n\npeer".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"two\nlines")),
    //       Gap(List(), 2), Node(t"peer")))

    //   test(t"Single node with param"):
    //     Codl.parse(t"root param".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"param"))))
      
    //   test(t"Single node with param and extra padding"):
    //     Codl.parse(t"root  param".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"param"))))
      
    //   test(t"Single node with param and child"):
    //     Codl.parse(t"root param\n  child".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"param"), IArray(Node(t"child")))))
      
    //   test(t"Single node with param and child with comment"):
    //     Codl.parse(t"root param\n  #comment\n  child".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"param"),
    //       IArray(Node(t"child", Map(), IArray(), Meta(List(t"comment"), 0, None))))))
      
    //   test(t"Single node with param and child with multiword comment"):
    //     Codl.parse(t"root param\n  #some comment\n  child".s.stripMargin.show).children.to(List)
    //   .assert(_ == List(Node(t"root", Map(SpecialKey.UntypedNode -> t"param"),
    //       IArray(Node(t"child", Map(), IArray(), Meta(List(t"some comment"), 0, None))))))
      
    //   test(t"Parse simple tree and get root"):
    //     Codl.parse(t"""|root
    //                    |  child
    //                    |""".s.stripMargin.show).children.head
    //   .matches:
    //     case Node(t"root", _, _, _) =>
      
    //   test(t"Parse simple tree and get child"):
    //     Codl.parse(t"""|root
    //                    |  child
    //                    |""".s.stripMargin.show).children.head.children.head
    //   .matches:
    //     case Node(t"child", _, _, _) =>
      
    //   test(t"Parse more deeply-nested child"):
    //     Codl.parse(t"""|root
    //                    |  child
    //                    |    node
    //                    |""".s.stripMargin.show).children.head.children.head.children.head
    //   .matches:
    //     case Node(t"node", _, _, _) =>
      
    //   test(t"Allow prefix on first line"):
    //     Codl.parse(t"""|  root
    //                    |    child
    //                    |      node
    //                    |""".s.stripMargin.show).children.head.children.head.children.head
    //   .matches:
    //     case Node(t"node", _, _, _) =>
      
    //   test(t"Allow odd prefix on first line"):
    //     Codl.parse(t"""|   root
    //                    |     child
    //                    |       node
    //                    |""".s.stripMargin.show).children.head.children.head.children.head
    //   .matches:
    //     case Node(t"node", _, _, _) =>

    //   test(t"Only one space of indentation fails"):
    //     capture:
    //       Codl.parse(t"""|root
    //                      | child
    //                      |""".s.stripMargin.show)
    //   .matches:
    //     case AggregateError(List(CodlParseError(6, UnevenIndent(_, _)))) =>
      
    //   test(t"Indentation less than initial prefix is an error"):
    //     capture:
    //       Codl.parse(t"""|    root
    //                      |      child
    //                      |  broken
    //                      |""".s.stripMargin.show)
    //   .matches:
    //     case AggregateError(List(CodlParseError(23, InsufficientIndent))) =>

    //   test(t"Initial prefix can be after newline"):
    //     Codl.parse(t"""
    //                   root
    //                     child
    //                     """).children.head.children.head
    //   .matches:
    //     case Node(t"child", _, _, _) =>
      
    //   test(t"Last line does not need to be terminated"):
    //     Codl.parse(t"root")
    //   .assert(_.children.length == 1)
      
    //   test(t"Comment is not a node"):
    //     Codl.parse(t"""
    //       #comment
    //       root
    //       """)
    //   .assert(_.children.length == 1)
      
    //   test(t"Comment may be child"):
    //     println(t"Comment may be child")
    //     Codl.parse(t"""
    //       node1
    //         #comment
    //       node2
    //       """)
    //   .assert(_.children.length == 2)
      
    //   test(t"Empty string is empty document"):
    //     println(t"Empty string is empty document")
    //     Codl.parse(t"")
    //   .assert(_.children.length == 0)

    //   test(t"Document starts with comment"):
    //     Codl.parse(t"""|#!/bin/bash
    //                    |roots
    //                    |""".s.stripMargin.show)
    //   .assert(_.children.length == 1)
      
    //   test(t"Unindent after comment forbidden"):
    //     println("Unindent test")
    //     capture:
    //       Codl.parse(t"""|  #comment
    //                      |root
    //                      |""".s.stripMargin.show)
    //   .matches:
    //     case AggregateError(List(CodlParseError(11, InsufficientIndent))) =>
      
    //   test(t"root node with params"):
    //     Codl.parse(t"root param1 param2").children.head.params.toString.show
    //   .assert(_ == t"")
      
    //   test(t"root node with children counts params correctly"):
    //     Codl.parse(t"""
    //       root param1 param2
    //         child1
    //         child2
    //     """).children.head.params.size
    //   .assert(_ == 2)
      
    //   test(t"root node with params counts children correctly"):
    //     Codl.parse(t"""
    //       root param1 param2
    //         child1
    //         child2
    //     """).children.head.children.size
    //   .assert(_ == 2)
      
    //   test(t"child node with param"):
    //     Codl.parse(t"""
    //       root param1 param2
    //         child1 param1
    //     """).children.head.children.head.params.size
    //   .assert(_ == 1)
      
    //   test(t"child node with param and trailing comment"):
    //     Codl.parse(t"""
    //       root param1 param2
    //         child1 param1 # line comment
    //     """).children.head.children.head.params.size
    //   .assert(_ == 1)
      
    //   test(t"misaligned comments"):
    //     capture:
    //       Codl.parse(t"""
    //         root param
    //           # comment 1
    //             # comment 2
    //             child1
    //       """)
    //   .assert(_ == AggregateError(List(CodlParseError(66, IndentAfterComment))))
      
    //   test(t"comment not aligned with associated child"):
    //     capture:
    //       Codl.parse(t"""
    //         root param
    //           # comment 1
    //             child1
    //       """)
    //   .assert(_ == AggregateError(List(CodlParseError(66, IndentAfterComment))))
      
    //   test(t"unindent on comment permitted"):
    //     Codl.parse(t"""
    //       root param
    //         child1
    //           grandchild1
    //         # unindented comment
    //     """)
    //   .assert { _ => true }

      // test(t"single unindent"):
      //   Schema.Freeform.parse(t"""|root
      //                             |  child1
      //                             |    grandchild1
      //                             |  child2
      //                             |""".s.stripMargin.show).root().child2()
      // .matches:
      //   case Data(t"child2", Nil, _) =>
      
      // test(t"double unindent"):
      //   Schema.Freeform.parse(t"""|root
      //                             |  child
      //                             |    grandchild
      //                             |root2
      //                             |""".s.stripMargin.show).root2()
      // .matches:
      //   case Data(t"root2", Nil, _) =>

      // test(t"indented param"):
      //   Schema.Freeform.parse(t"""
      //     root
      //         Long text
      //   """).root()(0)
      // .assert(_ == Node(t"Long text")())
      
      // test(t"indented param then peer"):
      //   Schema.Freeform.parse(t"""
      //     root
      //         Long text
      //     peer
      //   """).root()(0)
      // .assert(_ == Node(t"Long text")())
      
      // test(t"indented param then child"):
      //   Schema.Freeform.parse(t"""
      //     root
      //         Long text
      //       child
      //   """).root()(0)
      // .assert(_ == Node(t"Long text")())
      
      // test(t"multiline param"):
      //   Schema.Freeform.parse(t"""
      //     root
      //         Line 1
      //         Line 2
      //       child
      //   """).root()(0)
      // .assert(_ == Node(t"Line 1\nLine 2")())
      
      // test(t"multiline param allows uneven indentation"):
      //   Schema.Freeform.parse(t"""
      //     root
      //         Line 1
      //          Line 2
      //       child
      //   """).root()(0)
      // .assert(_ == Node(t"Line 1\n Line 2")())
      
      // test(t"multiline param allows extra even indentation"):
      //   Schema.Freeform.parse(t"""
      //     root
      //         Line 1
      //           Line 2
      //       child
      //   """).root()(0)
      // .assert(_ == Node(t"Line 1\n  Line 2")())
      
      // test(t"indented param can be last thing in doc"):
      //   Schema.Freeform.parse(t"""
      //     root
      //         Long text""").root()(0)
      // .assert(_ == Node(t"Long text")())
      
      // test(t"multiline param includes trailing spaces"):
      //   import logging.stdout
      //   Schema.Freeform.parse(t"""
      //     root
      //         Long text   
      //   """).root()(0)
      // .assert(_ == Node(t"Long text   ")())
      
      // test(t"multiline param excludes trailing newline"):
      //   Schema.Freeform.parse(t"""
      //     root
      //         Long text

      //   """).root()(0)
      // .assert(_ == Node(t"Long text")())
      
      // test(t"multiline param is not a comment"):
      //   Schema.Freeform.parse(t"""
      //     root
      //         # Long text
      //   """).root()(0)
      // .assert(_ == Node(t"# Long text")())
      
      // test(t"trailing comment is not a param"):
      //   Schema.Freeform.parse(t"""
      //     root
      //       child abc # comment
      //   """).root().child
      // .assert(_ == t"")
      
      // test(t"trailing comment is accessible"):
      //   Schema.Freeform.parse(t"""
      //     root
      //       child abc # comment
      //   """).root().child
      // .assert(_ == t"comment")
      
      // test(t"leading comment is accessible"):
      //   Schema.Freeform.parse(t"""
      //     root
      //       # message
      //       child abc
      //   """).root()(0)
      // .matches:
      //   case Node(_, Meta(_, List(t" message"), _, _)) =>
      
      // test(t"leading comment is two lines long"):
      //   Schema.Freeform.parse(t"""
      //     root
      //       # line 1
      //       # line 2
      //       child abc
      //   """).root().children.head
      // .matches:
      //   case Node(_, Meta(0, List(t" line 1", t" line 2"), _, _)) =>
      
    //   test(t"blank line separates comments"):
    //     Codl.parse(t"""
    //       root
    //         # line 1

    //         # line 2
    //         child abc
    //     """).children.head.children(1)
    //   .matches:
    //     case Node(_, _, _, Meta(List(t" line 2"), _, _)) =>
      
    //   test(t"standalone comment is accessible"):
    //     Codl.parse(t"""
    //       root
    //         # line 1

    //         # line 2
    //         child abc
    //     """).children.head.children.head
    //   .matches:
    //     case Gap(List(t" line 1"), 1) =>
      
    //   test(t"blank lines between peers are counted"):
    //     Codl.parse(t"""
    //       root


    //       peer abc
    //     """).children(1)
    //   .matches:
    //     case Gap(List(), 2) =>
      
    //   test(t"blank lines are counted"):
    //     Codl.parse(t"""
    //       root


    //         child abc
    //     """).children.head.children.head
    //   .matches:
    //     case Gap(_, 2) =>

    //suite(t"Schema tests"):

      // val basicSchema = SchemaDoc.parse(t"""
      //   root
      //     item
      //       value?
      //       element? param
      //       option* id!
      //     other?
      // """)
      // val basicSchema = Schema(ListMap(
      //   t"root" -> Schema(ListMap(
      //     t"item" -> Schema(ListMap(
      //       t"value"   -> Schema(ListMap(), Multiplicity.Optional),
      //       t"element" -> Schema(ListMap(t"param" -> Schema(ListMap(), Multiplicity.Optional))),
      //       t"option"  -> Schema(ListMap(t"id" -> Schema(ListMap(), Multiplicity.Unique)), Multiplicity.Many)
      //     )),
      //     t"other" -> Schema(ListMap(), Multiplicity.Optional)
      //   ))
      // ))

      // test(t"Simple schema structure"):
      //   basicSchema.parse(t"""
      //     root
      //       item
      //         value
      //   """)
      // .assert(_ => true)
      
      // test(t"Schema doesn't contain child"):
      //   capture:
      //     basicSchema.parse(t"""
      //       root
      //         child
      //     """)
      // .matches:
      //   case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.InvalidKey(t"child")))) =>
        
    //   test(t"Required value must be included"):
    //     capture:
    //       basicSchema.parse(t"""
    //         root
    //       """)
    //   .matches:
    //     case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.MissingKey(t"item")))) =>
        
    //   test(t"unique value should not be duplicated"):
    //     capture:
    //       basicSchema.parse(t"""
    //         root
    //           item
    //             value
    //             value
    //       """)
    //   .matches:
    //     case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.DuplicateKey(t"value", _)))) =>
      
    //   test(t"unique value with param should not be duplicated"):
    //     capture:
    //       basicSchema.parse(t"""
    //         root
    //           item
    //             element one
    //             element two
    //       """)
    //   .matches:
    //     case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.DuplicateKey(t"element", _)))) =>

    //   test(t"required param is missing"):
    //     capture:
    //       basicSchema.parse(t"""
    //         root
    //           item
    //             element
    //       """)
    //   .matches:
    //     case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.MissingKey(t"param")))) =>

    //   test(t"too many parameters"):
    //     capture:
    //       basicSchema.parse(t"""
    //         root
    //           item
    //             element one two
    //       """)
    //   .matches:
    //     case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.SurplusParams))) =>
      
    //   test(t"duplicated ID"):
    //     capture:
    //       basicSchema.parse(t"""
    //         root
    //           item
    //             option one
    //             option one
    //       """)
    //   .matches:
    //     case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.DuplicateId(t"one")))) =>
      
    //   def compare(schema: SchemaDoc, src: Text)(using Log): (Text, Text) =
    //     val result: Text = schema.parse(src).show
    //     src.sub(t" ", t"_") -> schema.parse(src).show.sub(t" ", t"_")

    //   test(t"Simple node with child"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)
      
    //   test(t"Simple node with parameter"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |    element argument
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)
      
    //   test(t"Simple node with indentation"):
    //     compare(basicSchema, t"""|   root
    //                              |     item
    //                              |       element value
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)
      
    //   test(t"Simple node with padded parameter"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |    element   argument
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)
      
    //   test(t"Simple node with trailing comment"):
    //     compare(basicSchema, t"""|root
    //                              |  item # comment
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)
      
    //   test(t"Simple node with trailing comment and extra space"):
    //     compare(basicSchema, t"""|root
    //                              |  item    # comment
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)

    //   test(t"Simple node with long parameter"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |    element
    //                              |        This is some simple text.
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)

    //   test(t"Simple node with inital comment"):
    //     import logging.stdout
    //     compare(basicSchema, t"""|#!/bin/bash
    //                              |root
    //                              |  item
    //                              |    element
    //                              |        This is some text.
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)

    //   test(t"Simple node with blank lines"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |
    //                              |    element
    //                              |        This is some text.
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)
      
    //   test(t"Serialize more complex structure"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |    value
    //                              |    # here's a comment
    //                              |    element abc
    //                              |    option xyz
    //                              |  other
    //                              |""".s.stripMargin.show)
    //   .assert(_ == _)
      
    //   val listSchema = SchemaDoc.parse(t"""
    //     root
    //       item* id!
    //   """)
      
    //   test(t"Serialize list of items"):
    //     compare(listSchema, t"""|root
    //                             |  item one
    //                             |  item two
    //                             |  item three
    //                             |""".s.stripMargin.show)
    //   .assert(_ == _)

    // case class Item(value: Text)
    // case class Items[T](item: T*)
    // case class Person(name: Text, age: Int)
    // case class Organisation(name: Text, ceo: Person)
    // case class Drink(name: Text)
    // case class Food(name: Text)
    // case class Meal(drink: List[Drink], food: List[Food])

    // suite(t"Generic Derivation tests"):
    //   test(t"write a simple case class"):
    //     Person(t"John Smith", 65).codl
    //   .matches:
    //     case Doc(_, IArray(Node(t"name", m1, _, _), Node(t"age", m2, _, _)), _)
    //       if m1.values == Iterable(t"John Smith") && m2.values == Iterable(t"65") =>
      
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
      