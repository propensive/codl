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
      import Tokenizer.Token, Token.*

      def parseText(text: Text): List[Tokenizer.Token] =
        Tokenizer(StringReader(text.s)).stream().to(List)

      test(t"Parse word"):
        parseText(t"root")
      .assert(_ == List(Word(t"root", 0)))
      
      test(t"Parse two words with single space"):
        parseText(t"alpha beta")
      .assert(_ == List(Word(t"alpha", 0), Padding(1, 5), Word(t"beta", 6)))
      
      test(t"Parse two words with trailing spaces"):
        parseText(t"alpha beta   ")
      .assert(_ == List(Word(t"alpha", 0), Padding(1, 5), Word(t"beta", 6), Padding(3, 10)))
      
      test(t"Parse two words with three spaces"):
        parseText(t"alpha   beta")
      .assert(_ == List(Word(t"alpha", 0), Padding(3, 5), Word(t"beta", 8)))
      
      test(t"Parse two words with newline"):
        parseText(t"alpha beta\n")
      .assert(_ == List(Word(t"alpha", 0), Padding(1, 5), Word(t"beta", 6), Newline(10)))

      test(t"Parse two words with two lines"):
        parseText(t"alpha\nbeta")
      .assert(_ == List(Word(t"alpha", 0), Newline(5), Word(t"beta", 6)))

      test(t"Parse two words on two lines with indentation"):
        parseText(t"alpha\n  beta")
      .assert(_ == List(Word(t"alpha", 0), Newline(5), Padding(2, 6), Word(t"beta", 8)))

      test(t"Parse two words on two lines with initial indentation"):
        parseText(t" alpha\n  beta")
      .assert(_ == List(Padding(1, 0), Word(t"alpha", 1), Newline(6), Padding(2, 7),
          Word(t"beta", 9)))
      
      test(t"Parse two words on two lines with initial newline"):
        parseText(t"\nalpha\n  beta")
      .assert(_ == List(Newline(0), Word(t"alpha", 1), Newline(6), Padding(2, 7),
          Word(t"beta", 9)))
      
      test(t"Parse text with whitespace on blank lines"):
        parseText(t"root\n    two\n\n \npeer")
      .assert(_ == List(Word(t"root", 0), Newline(4), Padding(4, 5), Word(t"two", 9),
          Newline(12), Newline(13), Padding(1, 14), Newline(15), Word(t"peer", 16)))
        
      test(t"Parse initial comment"):
        parseText(t"""|#!/bin/bash
                      |root
                      |""".s.stripMargin.show)
      .assert(_ == List(Word(t"#!/bin/bash", 0), Newline(11), Word(t"root", 12), Newline(16)))
      
    suite(t"Parsing tests"):
      import Line.*
      test(t"Parse smallest document"):
        Codl.parse(t"""root""".s.stripMargin.show).children
      .assert(_ == List(Node(t"root")))
      
      test(t"Parse top-level peers"):
        Codl.parse(t"root\nnext".s.stripMargin.show).children
      .assert(_ == List(Node(t"root"), Node(t"next")))
      
      test(t"Parse root and one child"):
        Codl.parse(t"root\n  child".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, None, List(Node(t"child")))))
      
      test(t"Parse root and two children"):
        Codl.parse(t"root\n  child1\n  child2".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, None, List(Node(t"child1"), Node(t"child2")))))
      
      test(t"Parse root and two children with outdent"):
        Codl.parse(t"root\n  child1\n  child2\ntoplevel".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, None, List(Node(t"child1"), Node(t"child2"))), Node(t"toplevel")))

      test(t"Parse root with double outdent"):
        Codl.parse(t"one\n  child\n    grandchild\ntwo".s.stripMargin.show).children
      .assert(_ == List(Node(t"one", Nil, None, None, List(Node(t"child", Nil, None, None, List(Node(t"grandchild"))))), Node(t"two")))
      
      test(t"Single node with comment"):
        Codl.parse(t"#xyz\none".s.stripMargin.show).children
      .assert(_ == List(Node(t"one", Nil, None, None, Nil, List(t"xyz"))))
      
      test(t"Single node with comment and blank line in between"):
        Codl.parse(t"#xyz\n\none".s.stripMargin.show).children
      .assert(_ == List(Gap(List(t"xyz"), 1), Node(t"one", Nil, None, None, Nil, Nil)))
      
      test(t"Single node with 'long' one-word param"):
        Codl.parse(t"root\n    param".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"param")))))
      
      test(t"Single node with long multi-word param"):
        Codl.parse(t"root\n    The quick brown fox".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"The quick brown fox")))))
      
      test(t"Single node with long multi-line param"):
        Codl.parse(t"root\n    The quick brown fox\n    jumps over the lazy dog.".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"The quick brown fox\njumps over the lazy dog.")))))
      
      test(t"Single node with multiline-param and indentation"):
        Codl.parse(t"root\n    The quick brown fox\n     jumps over the lazy dog.".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"The quick brown fox\n jumps over the lazy dog.")))))
      
      test(t"Single node with multiline-param then child"):
        Codl.parse(t"root\n    two\n    lines\n  child".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines")), List(Node(t"child")))))
    
      test(t"Single node with multiline-param then peer"):
        Codl.parse(t"root\n    two\n    lines\npeer".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines"))), Node(t"peer")))
      
      test(t"Single node with multiline-param then blank line, then peer"):
        Codl.parse(t"root\n    two\n    lines\n\npeer".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines"))), Gap(Nil, 1), Node(t"peer")))
      
      test(t"Single node with multiline-param then two blank lines, then peer"):
        Codl.parse(t"root\n    two\n    lines\n\n\npeer".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines"))), Gap(Nil, 2), Node(t"peer")))
      
      test(t"Single node with multiline-param then two blank lines with surplus whitespace, then peer"):
        Codl.parse(t"root\n    two\n    lines\n\n\npeer".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines"))), Gap(Nil, 2), Node(t"peer")))

      test(t"Single node with param"):
        Codl.parse(t"root param".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 1, t"param")))))
      
      test(t"Single node with param and extra padding"):
        Codl.parse(t"root  param".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 2, t"param")))))
      
      test(t"Single node with param and child"):
        Codl.parse(t"root param\n  child".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 1, t"param")), None, None, List(Node(t"child")))))
      
      test(t"Single node with param and child with comment"):
        Codl.parse(t"root param\n  #comment\n  child".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 1, t"param")), None, None, List(Node(t"child", comments = List(t"comment"))))))
      
      test(t"Single node with param and child with multiword comment"):
        Codl.parse(t"root param\n  #some comment\n  child".s.stripMargin.show).children
      .assert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 1, t"param")), None, None, List(Node(t"child", comments = List(t"some comment"))))))
      
      test(t"Parse simple tree and get root"):
        Codl.parse(t"""|root
                       |  child
                       |""".s.stripMargin.show).children.head
      .matches:
        case Node(t"root", _, _, _, _, _) =>
      
      test(t"Parse simple tree and get child"):
        Codl.parse(t"""|root
                       |  child
                       |""".s.stripMargin.show).children.head.children.head
      .matches:
        case Node(t"child", _, _, _, _, _) =>
      
      test(t"Parse more deeply-nested child"):
        Codl.parse(t"""|root
                       |  child
                       |    node
                       |""".s.stripMargin.show).children.head.children.head.children.head
      .matches:
        case Node(t"node", _, _, _, _, _) =>
      
      test(t"Allow prefix on first line"):
        Codl.parse(t"""|  root
                       |    child
                       |      node
                       |""".s.stripMargin.show).children.head.children.head.children.head
      .matches:
        case Node(t"node", _, _, _, _, _) =>
      
      test(t"Allow odd prefix on first line"):
        Codl.parse(t"""|   root
                       |     child
                       |       node
                       |""".s.stripMargin.show).children.head.children.head.children.head
      .matches:
        case Node(t"node", _, _, _, _, _) =>

      test(t"Only one space of indentation fails"):
        capture:
          Codl.parse(t"""|root
                         | child
                         |""".s.stripMargin.show)
      .matches:
        case AggregateError(List(CodlParseError(6, UnevenIndent(_, _)))) =>
      
      test(t"Indentation less than initial prefix is an error"):
        capture:
          Codl.parse(t"""|    root
                         |      child
                         |  broken
                         |""".s.stripMargin.show)
      .matches:
        case AggregateError(List(CodlParseError(23, InsufficientIndent))) =>

      test(t"Initial prefix can be after newline"):
        Codl.parse(t"""
                      root
                        child
                        """).children.head.children.head
      .matches:
        case Node(t"child", _, _, _, _, _) =>
      
      test(t"Last line does not need to be terminated"):
        Codl.parse(t"root")
      .assert(_.children.length == 1)
      
      test(t"Comment is not a node"):
        Codl.parse(t"""
          #comment
          root
          """)
      .assert(_.children.length == 1)
      
      test(t"Comment may be child"):
        println(t"Comment may be child")
        Codl.parse(t"""
          node1
            #comment
          node2
          """)
      .assert(_.children.length == 2)
      
      test(t"Empty string is empty document"):
        println(t"Empty string is empty document")
        Codl.parse(t"")
      .assert(_.children.length == 0)

      test(t"Document starts with comment"):
        Codl.parse(t"""|#!/bin/bash
                       |roots
                       |""".s.stripMargin.show)
      .assert(_.children.length == 1)
      
      test(t"Unindent after comment forbidden"):
        println("Unindent test")
        capture:
          Codl.parse(t"""|  #comment
                         |root
                         |""".s.stripMargin.show)
      .matches:
        case AggregateError(List(CodlParseError(11, InsufficientIndent))) =>
      
      test(t"root node with params"):
        Codl.parse(t"root param1 param2").children.head.params.length
      .assert(_ == 2)
      
      test(t"root node with children counts params correctly"):
        Codl.parse(t"""
          root param1 param2
            child1
            child2
        """).children.head.params
      .assert(_.length == 2)
      
      test(t"root node with params counts children correctly"):
        Codl.parse(t"""
          root param1 param2
            child1
            child2
        """).children.head.children
      .assert(_.length == 2)
      
      test(t"child node with param"):
        Codl.parse(t"""
          root param1 param2
            child1 param1
        """).children.head.children.head.params
      .assert(_.length == 1)
      
      test(t"child node with param and trailing comment"):
        Codl.parse(t"""
          root param1 param2
            child1 param1 # line comment
        """).children.head.children.head.params
      .assert(_.length == 1)
      
      test(t"misaligned comments"):
        capture:
          Codl.parse(t"""
            root param
              # comment 1
                # comment 2
                child1
          """)
      .assert(_ == AggregateError(List(CodlParseError(66, IndentAfterComment))))
      
      test(t"comment not aligned with associated child"):
        capture:
          Codl.parse(t"""
            root param
              # comment 1
                child1
          """)
      .assert(_ == AggregateError(List(CodlParseError(66, IndentAfterComment))))
      
      test(t"unindent on comment permitted"):
        Codl.parse(t"""
          root param
            child1
              grandchild1
            # unindented comment
        """)
      .assert { _ => true }

      test(t"single unindent"):
        Codl.parse(t"""|root
                       |  child1
                       |    grandchild1
                       |  child2
                       |""".s.stripMargin.show).children.head.children(1)
      .matches:
        case Node(t"child2", _, _, _, _, _) =>
      
      test(t"double unindent"):
        Codl.parse(t"""|root
                       |  child
                       |    grandchild
                       |root2
                       |""".s.stripMargin.show).children(1)
      .matches:
        case Node(t"root2", _, _, _, _, _) =>

      test(t"indented param"):
        Codl.parse(t"""
          root
              Long text
            child
        """).children.head.asInstanceOf[Node].content.get.value
      .assert(_ == t"Long text")
      
      test(t"multiline param"):
        Codl.parse(t"""
          root
              Line 1
              Line 2
            child
        """).children.head.asInstanceOf[Node].content.get.value
      .assert(_ == t"Line 1\nLine 2")
      
      test(t"multiline param allows uneven indentation"):
        Codl.parse(t"""
          root
              Line 1
               Line 2
            child
        """).children.head.asInstanceOf[Node].content.get.value
      .assert(_ == t"Line 1\n Line 2")
      
      test(t"indented param can be last thing in doc"):
        Codl.parse(t"""
          root
              Long text
        """).children.head.asInstanceOf[Node].content.get.value
      .assert(_ == t"Long text")
      
      test(t"multiline param includes trailing spaces"):
        Codl.parse(t"""
          root
              Long text   
        """).children.head.asInstanceOf[Node].content.get.value
      .assert(_ == t"Long text   ")
      
      test(t"multiline param excludes trailing newline"):
        Codl.parse(t"""
          root
              Long text

        """).children.head.asInstanceOf[Node].content.get.value
      .assert(_ == t"Long text")
      
      test(t"multiline param is not a comment"):
        Codl.parse(t"""
          root
              # Long text
        """).children.head.asInstanceOf[Node].content.get.value
      .assert(_ == t"# Long text")
      
      test(t"trailing comment is not a param"):
        Codl.parse(t"""
          root
            child abc # comment
        """).children.head.children.head.params
      .assert(_.length == 1)
      
      test(t"trailing comment is accessible"):
        Codl.parse(t"""
          root
            child abc # comment
        """).children.head.children.head
      .matches:
        case Node(_, _, Some(Value(_, _, t"comment")), _, _, _) =>
      
      test(t"leading comment is accessible"):
        Codl.parse(t"""
          root
            # message
            child abc
        """).children.head.children.head
      .matches:
        case Node(_, _, _, _, _, List(t" message")) =>
      
      test(t"leading comment is two lines long"):
        Codl.parse(t"""
          root
            # line 1
            # line 2
            child abc
        """).children.head.children.head
      .matches:
        case Node(_, _, _, _, _, List(t" line 1", t" line 2")) =>
      
      test(t"blank line separates comments"):
        Codl.parse(t"""
          root
            # line 1

            # line 2
            child abc
        """).children.head.children(1)
      .matches:
        case Node(_, _, _, _, _, List(t" line 2")) =>
      
      test(t"standalone comment is accessible"):
        Codl.parse(t"""
          root
            # line 1

            # line 2
            child abc
        """).children.head.children.head
      .matches:
        case Gap(List(t" line 1"), 1) =>
      
      test(t"blank lines between peers are counted"):
        Codl.parse(t"""
          root


          peer abc
        """).children(1)
      .matches:
        case Gap(Nil, 2) =>
      
      test(t"blank lines are counted"):
        Codl.parse(t"""
          root


            child abc
        """).children.head.children.head
      .matches:
        case Gap(_, 2) =>

    suite(t"Schema tests"):

      val basicSchema = Subschema.parse(Codl.tokenize(StringReader(t"""
        root
          item
            value?
            element? param
            option* id!
          other?
      """.s))(0))

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
        
      test(t"Required value must be included"):
        capture:
          basicSchema.parse(t"""
            root
          """)
      .matches:
        case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.MissingKey(t"item")))) =>
        
      test(t"unique value should not be duplicated"):
        capture:
          basicSchema.parse(t"""
            root
              item
                value
                value
          """)
      .matches:
        case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.DuplicateKey(t"value", _)))) =>
      
      test(t"unique value with param should not be duplicated"):
        capture:
          basicSchema.parse(t"""
            root
              item
                element one
                element two
          """)
      .matches:
        case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.DuplicateKey(t"element", _)))) =>

      test(t"required param is missing"):
        capture:
          basicSchema.parse(t"""
            root
              item
                element
          """)
      .matches:
        case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.MissingKey(t"param")))) =>

      test(t"too many parameters"):
        capture:
          basicSchema.parse(t"""
            root
              item
                element one two
          """)
      .matches:
        case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.SurplusParams))) =>
      
      test(t"duplicated ID"):
        capture:
          basicSchema.parse(t"""
            root
              item
                option one
                option one
          """)
      .matches:
        case AggregateError(List(CodlValidationError(_, CodlValidationError.Issue.DuplicateId(t"one")))) =>
      
    // val basicSchema = Subschema.parse(Codl.parse(t"""
    //   root
    //     child?
    //     item
    //       value?
    //       element? arg
    //       option* id!
    // """))
    
    // suite(t"Binary tests"):

    //   def roundtrip(doc: CodlDoc): CodlDoc = CodlDoc.read(doc.schema, doc.bin)

    //   test(t"simple structure"):
    //     val validDoc = basicSchema.validate(NextGen.parse(t"""
    //       root
    //         child
    //         item
    //           value
    //           element abc
    //     """))
      
    //   test(t"serialize simple structure"):
    //     val validDoc = basicSchema.validate(Codl.parse(t"""
    //       root
    //         child
    //         item
    //           value
    //           element abc
    //     """))
        
    //     validDoc.bin
    //   .assert(_ == t"""±ÀÚ!  "   ! "   ! ! #abc""")

    // suite(t"Serialization tests"):

      def compare(schema: SchemaDoc, src: Text)(using Log): (Text, Text) =
        val result: Text = schema.parse(src).show
        src.sub(t" ", t"_") -> schema.parse(src).show.sub(t" ", t"_")

      test(t"Simple node with child"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |""".s.stripMargin.show)
      .assert(_ == _)
      
      test(t"Simple node with parameter"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |    element argument
                                 |""".s.stripMargin.show)
      .assert(_ == _)
      
      test(t"Simple node with indentation"):
        compare(basicSchema, t"""|   root
                                 |     item
                                 |       element value
                                 |""".s.stripMargin.show)
      .assert(_ == _)
      
      test(t"Simple node with padded parameter"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |    element   argument
                                 |""".s.stripMargin.show)
      .assert(_ == _)
      
      test(t"Simple node with trailing comment"):
        compare(basicSchema, t"""|root
                                 |  item # comment
                                 |""".s.stripMargin.show)
      .assert(_ == _)
      
      test(t"Simple node with trailing comment and extra space"):
        compare(basicSchema, t"""|root
                                 |  item    # comment
                                 |""".s.stripMargin.show)
      .assert(_ == _)

      test(t"Simple node with long parameter"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |    element
                                 |        This is some simple text.
                                 |""".s.stripMargin.show)
      .assert(_ == _)

      test(t"Simple node with inital comment"):
        import logging.stdout
        compare(basicSchema, t"""|#!/bin/bash
                                 |root
                                 |  item
                                 |    element
                                 |        This is some text.
                                 |""".s.stripMargin.show)
      .assert(_ == _)

      test(t"Simple node with blank lines"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |
                                 |    element
                                 |        This is some text.
                                 |""".s.stripMargin.show)
      .assert(_ == _)
      
      test(t"Serialize more complex structure"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |    value
                                 |    # here's a comment
                                 |    element abc
                                 |    option xyz
                                 |  other
                                 |""".s.stripMargin.show)
      .assert(_ == _)

    case class Person(name: Text, age: Int)
    case class Organisation(name: Text, ceo: Person)

    suite(t"Generic Derivation tests"):
      test(t"write a simple case class"):
        Person(t"John Smith", 65).codl
      .matches:
        case Doc(_, List(
               Node(t"name", Nil, None, Some(Value(_, 1, t"John Smith")), _, _),
               Node(t"age", List(Value(_, 1, t"65")), _, _, _, _)
             ), _) =>
      
      test(t"write a nested case class"):
        Organisation(t"Acme Inc", Person(t"John Smith", 65)).codl.show
      .assert(_ == t"""|name
                       |    Acme Inc
                       |ceo
                       |  name
                       |      John Smith
                       |  age 65
                       |""".s.stripMargin.show)

      test(t"serialize a simple case class"):
        Person(t"John", 65).codl.show
      .assert(_ == t"""|name John
                       |age 65
                       |""".s.stripMargin.show)
      
      test(t"serialize a case class with long string"):
        Person(t"John Smith", 65).codl.show
      .assert(_ == t"""|name
                       |    John Smith
                       |age 65
                       |""".s.stripMargin.show)
      
      test(t"serialize a nested case class"):
        Organisation(t"Acme", Person(t"John", 65)).codl.show
      .assert(_ == t"""|name Acme
                       |ceo John 65
                       |""".s.stripMargin.show)

      test(t"serialize a nested case class and long strings"):
        Organisation(t"Acme Inc", Person(t"John Smith", 65)).codl.show
      .assert(_ == t"""|name
                       |    Acme Inc
                       |ceo
                       |  name
                       |      John Smith
                       |  age 65
                       |""".s.stripMargin.show)
    
    suite(t"Data roundtrip tests"):
      test(t"serialize and deserialize a case class"):
        val codl = Person(t"John Smith", 65).codl
        Codl.parse(codl.show).as[Person]
      .assert(_ == Person(t"John Smith", 65))
      
      test(t"roundtrip a nested case class with a long strings"):
        val org = Organisation(t"Acme Inc", Person(t"John Smith", 65))
        Codl.parse(org.codl.show).as[Organisation]
      .assert(_ == Organisation(t"Acme Inc", Person(t"John Smith", 65)))