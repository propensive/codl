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
    suite(t"Tokenizer tests"):
      def parseText(text: Text): LazyList[Word] = Tokenizer(StringReader(text.s)).stream()

      test(t"Parse word"):
        parseText(t"root")
      .assert(_ == LazyList(Word(t"root", Pos(0, 0))))
      
      test(t"Parse two words with single space"):
        parseText(t"alpha beta")
      .assert(_ == LazyList(Word(t"alpha", Pos(0, 0)), Word(t"beta", Pos(0, 6))))
      
      test(t"Parse two words with trailing spaces"):
        parseText(t"alpha beta   ")
      .assert(_ == LazyList(Word(t"alpha", Pos(0, 0)), Word(t"beta", Pos(0, 6))))
      
      test(t"Parse two words with three spaces"):
        parseText(t"alpha   beta")
      .assert(_ == LazyList(Word(t"alpha", Pos(0, 0)), Word(t"beta", Pos(0, 8))))
      
      test(t"Parse two words with newline"):
        parseText(t"alpha beta\n")
      .assert(_ == LazyList(Word(t"alpha", Pos(0, 0)), Word(t"beta", Pos(0, 6))))

      test(t"Parse two words with two lines"):
        parseText(t"alpha\nbeta")
      .assert(_ == LazyList(Word(t"alpha", Pos(0, 0)), Word(t"beta", Pos(1, 0))))

      test(t"Parse two words on two lines with indentation"):
        parseText(t"alpha\n  beta")
      .assert(_ == LazyList(Word(t"alpha", Pos(0, 0)), Word(t"beta", Pos(1, 2))))

      test(t"Parse two words on two lines with initial indentation"):
        parseText(t" alpha\n  beta")
      .assert(_ == LazyList(Word(t"alpha", Pos(0, 1)), Word(t"beta", Pos(1, 2))))
      
      test(t"Parse two words on two lines with initial newline"):
        parseText(t"\nalpha\n  beta")
      .assert(_ == LazyList(Word(t"alpha", Pos(1, 0)), Word(t"beta", Pos(2, 2))))
      
      test(t"Parse text with whitespace on blank lines"):
        parseText(t"root\n    two\n\n \npeer")
      .assert(_ == LazyList(Word(t"root", Pos(0, 0)), Word(t"two", Pos(1, 4)), Word(t"peer", Pos(4, 0))))
        
      test(t"Parse initial comment"):
        parseText(t"""|#!/bin/bash
                      |root
                      |""".s.stripMargin.show)
      .assert(_ == LazyList(Word(t"#!/bin/bash", Pos(0, 0)), Word(t"root", Pos(1, 0))))

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
    
    suite(t"Untyped parsing tests"):
      test(t"Empty document"):
        Schema.Freeform.parse(t"")
      .assert(_ == Doc())
      
      test(t"Simplest non-empty document"):
        Schema.Freeform.parse(t"root")
      .assert(_ == Doc(Node(Data(t"root"))))
      
      test(t"Root peers"):
        Schema.Freeform.parse(t"root\nelement\n")
      .assert(_ == Doc(Node(t"root")(), Node(t"element")()))

      test(t"Single child"):
        Schema.Freeform.parse(t"root\n  child")
      .assert(_ == Doc(Node(t"root")(Node(t"child")())))
      
      test(t"Single child and peer"):
        Schema.Freeform.parse(t"root\n  child\npeer")
      .assert(_ == Doc(Node(t"root")(Node(t"child")()), Node(t"peer")()))
      
      test(t"Single child and grandchild"):
        Schema.Freeform.parse(t"root\n  child\n    grandchild")
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"grandchild")()))))
      
      test(t"Single child and grandchild and peer"):
        Schema.Freeform.parse(t"root\n  child\n    grandchild\npeer")
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"grandchild")())), Node(t"peer")()))
      
      val schema = Schema(ListMap(
                     t"root" -> Schema(ListMap(
                       t"first" -> Schema(),
                       t"second" -> Schema(),
                       t"third" -> Schema(),
                       t"child" -> Schema(ListMap(
                         t"one" -> Schema(),
                         t"two" -> Schema()
                       )),
                       t"other" -> Schema(ListMap(
                         t"param" -> Schema(ListMap(), Multiplicity.Joined),
                       ))
                     ))
                   ))

      test(t"Root and param"):
        schema.parse(t"root param")
      .assert(_ == Doc(Node(t"root")(Node(t"first")(Node(t"param")()))))
      
      test(t"Root and two params"):
        schema.parse(t"root param1 param2")
      .assert(_ == Doc(Node(t"root")(Node(t"first")(Node(t"param1")()), Node(t"second")(Node(t"param2")()))))
      
      test(t"Root and three params"):
        schema.parse(t"root param1 param2 param3")
      .assert(_ == Doc(Node(t"root")(Node(t"first")(Node(t"param1")()), Node(t"second")(Node(t"param2")()), Node(t"third")(Node(t"param3")()))))
      
      test(t"Child with param"):
        schema.parse(t"root\n  child param3")
      .assert(_ == Doc(Node(t"root")(Node(t"child")(Node(t"one")(Node(t"param3")())))))
      
      test(t"Root with indent of 1"):
        Schema.Freeform.parse(t" root")
      .assert(_ == Doc(List(Node(t"root")()), 1))

      test(t"Single child with indent of 3"):
        Schema.Freeform.parse(t"   root\n     child")
      .assert(_ == Doc(List(Node(t"root")(Node(t"child")())), 3))
      
      test(t"Single child with joined parameter"):
        schema.parse(t"root\n  other Hello World")
      .assert(_ == Doc(Node(t"root")(Node(t"other")(Node(t"param")(Node(t"Hello World")())))))

      test(t"Parse peer root nodes (with indent)"):
        Schema.Freeform.parse(t"  root\n  peer")
      .assert(_ == Doc(List(Node(t"root")(), Node(t"peer")()), 2))
      
      test(t"Parse peer root node with parameters"):
        Schema.Freeform.parse(t"  root  param1 param2\n  peer")
      .assert(_ == Doc(List(Node(t"root")(Node(t"param1")(), Node(t"param2")()), Node(t"peer")()), 2))

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

      test(t"single unindent"):
        Schema.Freeform.parse(t"""|root
                                  |  child1
                                  |    grandchild1
                                  |  child2
                                  |""".s.stripMargin.show).root().child2()
      .matches:
        case Data(t"child2", Nil, _) =>
      
      test(t"double unindent"):
        Schema.Freeform.parse(t"""|root
                                  |  child
                                  |    grandchild
                                  |root2
                                  |""".s.stripMargin.show).root2()
      .matches:
        case Data(t"root2", Nil, _) =>

      test(t"indented param"):
        Schema.Freeform.parse(t"""
          root
              Long text
        """).root() //.children.head
      .assert(_ == Node(t"Long text")())
      
      test(t"indented param then peer"):
        import logging.stdout
        Schema.Freeform.parse(t"""
          root
              Long text
          peer
        """).root() //.children.head
      .assert(_ == Node(t"Long text")())
      
      test(t"indented param then child"):
        Schema.Freeform.parse(t"""
          root
              Long text
            child
        """).root().children.head
      .assert(_ == Node(t"Long text")())
      
      test(t"multiline param"):
        Schema.Freeform.parse(t"""
          root
              Line 1
              Line 2
            child
        """).root().children.head
      .assert(_ == Node(t"Line 1\nLine 2"))
      
    //   test(t"multiline param allows uneven indentation"):
    //     Codl.parse(t"""
    //       root
    //           Line 1
    //            Line 2
    //         child
    //     """).children.head.asInstanceOf[Node].params.last(1)
    //   .assert(_ == t"Line 1\n Line 2")
      
    //   test(t"indented param can be last thing in doc"):
    //     Codl.parse(t"""
    //       root
    //           Long text
    //     """).children.head.asInstanceOf[Node].params.last(1)
    //   .assert(_ == t"Long text")
      
    //   test(t"multiline param includes trailing spaces"):
    //     Codl.parse(t"""
    //       root
    //           Long text   
    //     """).children.head.asInstanceOf[Node].params.last(1)
    //   .assert(_ == t"Long text   ")
      
    //   test(t"multiline param excludes trailing newline"):
    //     Codl.parse(t"""
    //       root
    //           Long text

    //     """).children.head.asInstanceOf[Node].params.last(1)
    //   .assert(_ == t"Long text")
      
    //   test(t"multiline param is not a comment"):
    //     Codl.parse(t"""
    //       root
    //           # Long text
    //     """).children.head.asInstanceOf[Node].params.last(1)
    //   .assert(_ == t"# Long text")
      
    //   test(t"trailing comment is not a param"):
    //     Codl.parse(t"""
    //       root
    //         child abc # comment
    //     """).children.head.children.head.params.size
    //   .assert(_ == 1)
      
    //   test(t"trailing comment is accessible"):
    //     Codl.parse(t"""
    //       root
    //         child abc # comment
    //     """).children.head.children.head
    //   .matches:
    //     case Node(_, _, _, Meta(_, _, Some(t"comment"))) =>
      
    //   test(t"leading comment is accessible"):
    //     Codl.parse(t"""
    //       root
    //         # message
    //         child abc
    //     """).children.head.children.head
    //   .matches:
    //     case Node(_, _, _, Meta(List(t" message"), _, _)) =>
      
    //   test(t"leading comment is two lines long"):
    //     Codl.parse(t"""
    //       root
    //         # line 1
    //         # line 2
    //         child abc
    //     """).children.head.children.head
    //   .matches:
    //     case Node(_, _, _, Meta(List(t" line 1", t" line 2"), _, _)) =>
      
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

    suite(t"Schema tests"):

      // val basicSchema = SchemaDoc.parse(t"""
      //   root
      //     item
      //       value?
      //       element? param
      //       option* id!
      //     other?
      // """)
      val basicSchema = Schema(ListMap(
        t"root" -> Schema(ListMap(
          t"item" -> Schema(ListMap(
            t"value"   -> Schema(ListMap(), Multiplicity.Optional),
            t"element" -> Schema(ListMap(t"param" -> Schema(ListMap(), Multiplicity.Optional))),
            t"option"  -> Schema(ListMap(t"id" -> Schema(ListMap(), Multiplicity.Unique)), Multiplicity.Many)
          )),
          t"other" -> Schema(ListMap(), Multiplicity.Optional)
        ))
      ))

      test(t"Simple schema structure"):
        basicSchema.parse(t"""
          root
            item
              value
        """)
      .assert(_ => true)
      
      test(t"Schema doesn't contain child"):
        capture:
          basicSchema.parse(t"""
            root
              child
          """)
      .matches:
        case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.InvalidKey(t"child")))) =>
        
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
      