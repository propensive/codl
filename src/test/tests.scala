package codl

import probably.*
import gossamer.*
import rudiments.*
import turbulence.*
import eucalyptus.*

import java.io.StringReader

given Log(Everything |-> SystemOut)

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"CoDL tests"):
  def run(using Runner): Unit =
    suite(t"Tokenizer tests"):
      import Tokenizer.Token, Token.*

      def parseText(text: Text): List[Tokenizer.Token] =
        Tokenizer(StringReader(text.s)).stream().to(List)

      test(t"Parse word"):
        parseText(t"root")
      .oldAssert(_ == List(Word(t"root", 0)))
      
      test(t"Parse two words with single space"):
        parseText(t"alpha beta")
      .oldAssert(_ == List(Word(t"alpha", 0), Padding(1, 5), Word(t"beta", 6)))
      
      test(t"Parse two words with trailing spaces"):
        parseText(t"alpha beta   ")
      .oldAssert(_ == List(Word(t"alpha", 0), Padding(1, 5), Word(t"beta", 6), Padding(3, 10)))
      
      test(t"Parse two words with three spaces"):
        parseText(t"alpha   beta")
      .oldAssert(_ == List(Word(t"alpha", 0), Padding(3, 5), Word(t"beta", 8)))
      
      test(t"Parse two words with newline"):
        parseText(t"alpha beta\n")
      .oldAssert(_ == List(Word(t"alpha", 0), Padding(1, 5), Word(t"beta", 6), Newline(10)))

      test(t"Parse two words with two lines"):
        parseText(t"alpha\nbeta")
      .oldAssert(_ == List(Word(t"alpha", 0), Newline(5), Word(t"beta", 6)))

      test(t"Parse two words on two lines with indentation"):
        parseText(t"alpha\n  beta")
      .oldAssert(_ == List(Word(t"alpha", 0), Newline(5), Padding(2, 6), Word(t"beta", 8)))

      test(t"Parse two words on two lines with initial indentation"):
        parseText(t" alpha\n  beta")
      .oldAssert(_ == List(Padding(1, 0), Word(t"alpha", 1), Newline(6), Padding(2, 7),
          Word(t"beta", 9)))
      
      test(t"Parse two words on two lines with initial newline"):
        parseText(t"\nalpha\n  beta")
      .oldAssert(_ == List(Newline(0), Word(t"alpha", 1), Newline(6), Padding(2, 7),
          Word(t"beta", 9)))
      
      test(t"Parse text with whitespace on blank lines"):
        parseText(t"root\n    two\n\n \npeer")
      .oldAssert(_ == List(Word(t"root", 0), Newline(4), Padding(4, 5), Word(t"two", 9),
          Newline(12), Newline(13), Padding(1, 14), Newline(15), Word(t"peer", 16)))
      
    suite(t"Parsing tests"):
      import UntypedNode.*
      test(t"Parse smallest document"):
        Codl.parse(t"""root""".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root")))
      
      test(t"Parse top-level peers"):
        Codl.parse(t"root\nnext".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root"), Node(t"next")))
      
      test(t"Parse root and one child"):
        Codl.parse(t"root\n  child".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, None, List(Node(t"child")))))
      
      test(t"Parse root and two children"):
        Codl.parse(t"root\n  child1\n  child2".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, None, List(Node(t"child1"), Node(t"child2")))))
      
      test(t"Parse root and two children with outdent"):
        Codl.parse(t"root\n  child1\n  child2\ntoplevel".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, None, List(Node(t"child1"), Node(t"child2"))), Node(t"toplevel")))

      test(t"Parse root with double outdent"):
        Codl.parse(t"one\n  child\n    grandchild\ntwo".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"one", Nil, None, None, List(Node(t"child", Nil, None, None, List(Node(t"grandchild"))))), Node(t"two")))
      
      test(t"Single node with comment"):
        Codl.parse(t"#comment\none".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"one", Nil, None, None, Nil, List(t"comment"))))
      
      test(t"Single node with 'long' one-word param"):
        Codl.parse(t"root\n    param".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"param")))))
      
      test(t"Single node with long multi-word param"):
        Codl.parse(t"root\n    The quick brown fox".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"The quick brown fox")))))
      
      test(t"Single node with long multi-line param"):
        Codl.parse(t"root\n    The quick brown fox\n    jumps over the lazy dog.".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"The quick brown fox\njumps over the lazy dog.")))))
      
      test(t"Single node with multiline-param and indentation"):
        Codl.parse(t"root\n    The quick brown fox\n     jumps over the lazy dog.".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"The quick brown fox\n jumps over the lazy dog.")))))
      
      test(t"Single node with multiline-param then child"):
        Codl.parse(t"root\n    two\n    lines\n  child".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines")), List(Node(t"child")))))
    
      test(t"Single node with multiline-param then peer"):
        Codl.parse(t"root\n    two\n    lines\npeer".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines"))), Node(t"peer")))
      
      test(t"Single node with multiline-param then blank line, then peer"):
        Codl.parse(t"root\n    two\n    lines\n\npeer".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines"))), Gap(Nil, 1), Node(t"peer")))
      
      test(t"Single node with multiline-param then two blank lines, then peer"):
        Codl.parse(t"root\n    two\n    lines\n\n\npeer".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines"))), Gap(Nil, 2), Node(t"peer")))
      
      test(t"Single node with multiline-param then two blank lines with surplus whitespace, then peer"):
        Codl.parse(t"root\n    two\n    lines\n\n\npeer".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", Nil, None, Some(Value(SpecialKey.UntypedNode, 0, t"two\nlines"))), Gap(Nil, 2), Node(t"peer")))

      test(t"Single node with param"):
        Codl.parse(t"root param".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 1, t"param")))))
      
      test(t"Single node with param and extra padding"):
        Codl.parse(t"root  param".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 2, t"param")))))
      
      test(t"Single node with param and child"):
        Codl.parse(t"root param\n  child".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 1, t"param")), None, None, List(Node(t"child")))))
      
      test(t"Single node with param and child with comment"):
        Codl.parse(t"root param\n  #comment\n  child".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 1, t"param")), None, None, List(Node(t"child", comments = List(t"comment"))))))
      
      test(t"Single node with param and child with multiword comment"):
        Codl.parse(t"root param\n  #some comment\n  child".s.stripMargin.show).children
      .oldAssert(_ == List(Node(t"root", List(Value(SpecialKey.UntypedNode, 1, t"param")), None, None, List(Node(t"child", comments = List(t"some comment"))))))
      

      test(t"Parse simple tree and get root"):
        Codl.parse(t"""|root
                       |  child
                       |""".s.stripMargin.show).children.head.key
      .oldAssert(_ == t"root")
      
    //   test(t"Parse simple tree and get child"):
    //     Codl.parse(t"""|root
    //                    |  child
    //                    |""".s.stripMargin.show).children.head.children.head()
    //   .oldAssert(_ == t"child")
      
    //   test(t"Parse more deeply-nested child"):
    //     Codl.parse(t"""|root
    //                    |  child
    //                    |    node
    //                    |""".s.stripMargin.show).children.head.children.head.children.head()
    //   .oldAssert(_ == t"node")
      
    //   test(t"Allow prefix on first line"):
    //     Codl.parse(t"""|  root
    //                    |    child
    //                    |      node
    //                    |""".s.stripMargin.show).children.head.children.head.children.head()
    //   .oldAssert(_ == t"node")
      
    //   test(t"Allow odd prefix on first line"):
    //     Codl.parse(t"""|   root
    //                    |     child
    //                    |       node
    //                    |""".s.stripMargin.show).children.head.children.head.children.head()
    //   .oldAssert(_ == t"node")

    //   test(t"Only one space of indentation fails"):
    //     capture:
    //       Codl.parse(t"""|root
    //                      | child
    //                      |""".s.stripMargin.show)
    //   .matches:
    //     case CodlParseError(6, CodlParseError.Indentation.Uneven) =>
      
    //   test(t"Indentation less than initial prefix is an error"):
    //     capture:
    //       Codl.parse(t"""|    root
    //                       |      child
    //                       |   broken
    //                       |""".s.stripMargin.show)
    //   .matches:
    //     case CodlParseError(24, CodlParseError.Indentation.Insufficient) =>

    //   test(t"Initial prefix can be after newline"):
    //     Codl.parse(t"""
    //                   root
    //                     child
    //                     """).children.head.children.head()
    //   .oldAssert(_ == t"child")
      
    //   test(t"Last line does not need to be terminated"):
    //     Codl.parse(t"root")
    //   .oldAssert(_.children.length == 1)
      
    //   test(t"Comment is not a node"):
    //     Codl.parse(t"""
    //       #comment
    //       root
    //       """)
    //   .oldAssert(_.children.length == 1)
      
    //   test(t"Comment may be child"):
    //     Codl.parse(t"""
    //       node1
    //         #comment
    //       node2
    //       """)
    //   .oldAssert(_.children.length == 2)
      
    //   test(t"Empty string is empty document"):
    //     Codl.parse(t"")
    //   .oldAssert(_.children.length == 0)

    //   test(t"Document starts with comment"):
    //     Codl.parse(t"""|#comment
    //                     |root
    //                     |""".s.stripMargin.show)
    //   .oldAssert(_.children.length == 1)
      
    //   test(t"Unindent after comment forbidden"):
    //     capture:
    //       Codl.parse(t"""|  #comment
    //                       |root
    //                       |""".s.stripMargin.show)
    //   .matches:
    //     case CodlParseError(12, CodlParseError.Indentation.Insufficient) =>
      
    //   test(t"root node with params"):
    //     Codl.parse(t"root param1 param2").children.head.params.length
    //   .oldAssert(_ == 2)
      
    //   test(t"root node with children counts params correctly"):
    //     Codl.parse(t"""
    //       root param1 param2
    //         child1
    //         child2
    //     """).children.head.params
    //   .oldAssert(_.length == 2)
      
    //   test(t"root node with params counts children correctly"):
    //     Codl.parse(t"""
    //       root param1 param2
    //         child1
    //         child2
    //     """).children.head.children
    //   .oldAssert(_.length == 2)
      
    //   test(t"child node with param"):
    //     Codl.parse(t"""
    //       root param1 param2
    //         child1 param1
    //     """).children.head.children.head.params
    //   .oldAssert(_.length == 1)
      
    //   test(t"child node with param and trailing comment"):
    //     Codl.parse(t"""
    //       root param1 param2
    //         child1 param1 # line comment
    //     """).children.head.children.head.params
    //   .oldAssert(_.length == 1)
      
    //   test(t"misaligned comments"):
    //     capture:
    //       Codl.parse(t"""
    //         root param
    //           # comment 1
    //             # comment 2
    //             child1
    //       """)
    //   .oldAssert(_ == CodlParseError(66, CodlParseError.Indentation.AfterComment))
      
    //   test(t"comment not aligned with associated child"):
    //     capture:
    //       Codl.parse(t"""
    //         root param
    //           # comment 1
    //             child1
    //       """)
    //   .oldAssert(_ == CodlParseError(66, CodlParseError.Indentation.AfterComment))
      
    //   test(t"unindent on comment permitted"):
    //     Codl.parse(t"""
    //       root param
    //         child1
    //           grandchild1
    //         # unindented comment
    //     """)
    //   .oldAssert { _ => true }

    //   test(t"single unindent"):
    //     Codl.parse(t"""|root
    //                     |  child1
    //                     |    grandchild1
    //                     |  child2
    //                     |""".s.stripMargin.show).children.head.children(1)()
    //   .oldAssert(_ == t"child2")
      
    //   test(t"double unindent"):
    //     Codl.parse(t"""|root
    //                     |  child
    //                     |    grandchild
    //                     |root2
    //                     |""".s.stripMargin.show).children(1)()
    //   .oldAssert(_ == t"root2")

    //   test(t"indented param"):
    //     Codl.parse(t"""
    //       root
    //           Long text
    //         child
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Long text")
      
    //   test(t"multiline param"):
    //     Codl.parse(t"""
    //       root
    //           Line 1
    //           Line 2
    //         child
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Line 1\nLine 2")
      
    //   test(t"multiline param allows uneven indentation"):
    //     Codl.parse(t"""
    //       root
    //           Line 1
    //            Line 2
    //         child
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Line 1\n Line 2")
      
    //   test(t"indented param can be last thing in doc"):
    //     Codl.parse(t"""
    //       root
    //           Long text
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Long text")
      
    //   test(t"multiline param includes trailing spaces"):
    //     Codl.parse(t"""
    //       root
    //           Long text   
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Long text   ")
      
    //   test(t"multiline param excludes trailing newline"):
    //     Codl.parse(t"""
    //       root
    //           Long text

    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Long text")
      
    //   test(t"multiline param is not a comment"):
    //     Codl.parse(t"""
    //       root
    //           # Long text
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"# Long text")
      
    //   test(t"trailing comment is not a param"):
    //     Codl.parse(t"""
    //       root
    //         child abc # comment
    //     """).children.head.children.head.params
    //   .oldAssert(_.length == 1)
      
    //   test(t"trailing comment is accessible"):
    //     Codl.parse(t"""
    //       root
    //         child abc # comment
    //     """).children.head.children.head
    //   .matches:
    //     case Node.Data(_, _, _, Some(t"comment"), _) =>
      
    //   test(t"leading comment is accessible"):
    //     Codl.parse(t"""
    //       root
    //         # message
    //         child abc
    //     """).children.head.children.head.comment
    //   .oldAssert(_ == Some(t" message"))
      
    //   test(t"leading comment is two lines long"):
    //     Codl.parse(t"""
    //       root
    //         # line 1
    //         # line 2
    //         child abc
    //     """).children.head.children.head.comment
    //   .oldAssert(_ == Some(t" line 1\n line 2"))
      
    //   test(t"blank line separates comments"):
    //     Codl.parse(t"""
    //       root
    //         # line 1

    //         # line 2
    //         child abc
    //     """).children.head.children(1).comment
    //   .oldAssert(_ == Some(t" line 2"))
      
    //   test(t"standalone comment is accessible"):
    //     Codl.parse(t"""
    //       root
    //         # line 1

    //         # line 2
    //         child abc
    //     """).children.head.children.head.comment
    //   .oldAssert(_ == Some(t" line 1"))
      
    //   test(t"blank lines are counted"):
    //     Codl.parse(t"""
    //       root


    //         child abc
    //     """).children.head.children.head
    //   .matches:
    //     case Node.Blank(_, 2, _) =>

    // suite(t"Schema tests"):

    //   val basicSchema = Subschema.parse(Codl.parse(t"""
    //     root
    //       item
    //         value?
    //         element? param
    //         option* id!
    //   """))

    //   test(t"Simple schema structure"):
    //     basicSchema.validate(Codl.parse(t"""
    //       root
    //         item
    //           value
    //     """))
    //   .oldAssert(_ => true)
      
    //   test(t"Schema doesn't contain child"):
    //     capture:
    //       basicSchema.validate(Codl.parse(t"""
    //         root
    //           child
    //       """))
    //   .matches:
    //     case CodlValidationError(_, CodlValidationError.Issue.InvalidKey(t"child")) =>
        
    //   test(t"required value must be included"):
    //     capture:
    //       basicSchema.validate(Codl.parse(t"""
    //         root
    //       """))
    //   .matches:
    //     case CodlValidationError(_, CodlValidationError.Issue.MissingKey(t"item")) =>
        
    //   test(t"unique value should not be duplicated"):
    //     capture:
    //       basicSchema.validate(Codl.parse(t"""
    //         root
    //           item
    //             value
    //             value
    //       """))
    //   .matches:
    //     case CodlValidationError(_, CodlValidationError.Issue.DuplicateKey(t"value", _)) =>
      
    //   test(t"unique value with param should not be duplicated"):
    //     capture:
    //       basicSchema.validate(Codl.parse(t"""
    //         root
    //           item
    //             element one
    //             element two
    //       """))
    //   .matches:
    //     case CodlValidationError(_, CodlValidationError.Issue.DuplicateKey(t"element", _)) =>

    //   // test(t"required param is missing"):
    //   //   capture:
    //   //     basicSchema.validate(Codl.parse(t"""
    //   //       root
    //   //         item
    //   //           element
    //   //     """))
    //   // .matches:
    //   //   case CodlValidationError(_, _, CodlValidationError.Issue.MissingParams(1)) =>

    //   // test(t"too many parameters"):
    //   //   capture:
    //   //     basicSchema.validate(Codl.parse(t"""
    //   //       root
    //   //         item
    //   //           element one two
    //   //     """))
    //   // .matches:
    //   //   case CodlValidationError(_, CodlValidationError.Issue.SurplusParams) =>
      
    //   test(t"duplicated ID"):
    //     capture:
    //       basicSchema.validate(Codl.parse(t"""
    //         root
    //           item
    //             option one
    //             option one
    //       """))
    //   .matches:
    //     case CodlValidationError(_, CodlValidationError.Issue.DuplicateId(t"one")) =>
      
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
    //   .oldAssert(_ == t"""±ÀÚ!  "   ! "   ! ! #abc""")

    // suite(t"Serialization tests"):

    //   def compare(schema: Schema, src: Text): (Text, Text) =
    //     Log.info(t" Original: "+src.debug)
    //     Log.info(t"Parsed: "+Codl.parse(src).show)
    //     val result = schema.validate(Codl.parse(src)).show
    //     Log.info(t"Processed: "+result.debug)
    //     src.sub(t" ", t"_") -> schema.validate(Codl.parse(src)).show.sub(t" ", t"_")

    //   test(t"Simple node with child"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)
      
    //   test(t"Simple node with parameter"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |    element argument
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)
      
    //   test(t"Simple node with indentation"):
    //     compare(basicSchema, t"""|   root
    //                              |     item
    //                              |       element value
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)
      
    //   test(t"Simple node with padded parameter"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |    element   argument
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)
      
    //   test(t"Simple node with trailing comment"):
    //     compare(basicSchema, t"""|root
    //                              |  item # comment
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)
      
    //   test(t"Simple node with trailing comment and extra space"):
    //     compare(basicSchema, t"""|root
    //                              |  item    # comment
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)

    //   test(t"Simple node with long parameter"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |    element
    //                              |        This is some text.
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)

    //   test(t"Simple node with inital comment"):
    //     compare(basicSchema, t"""|#!/bin/bash
    //                              |root
    //                              |  item
    //                              |    element
    //                              |        This is some text.
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)

    //   test(t"Simple node with blank lines"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |
    //                              |    element
    //                              |        This is some text.
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)
      
    //   test(t"Serialize more complex structure"):
    //     compare(basicSchema, t"""|root
    //                              |  item
    //                              |    value
    //                              |    # here's a comment
    //                              |    element abc
    //                              |    option xyz
    //                              |  child
    //                              |""".s.stripMargin.show)
    //   .oldAssert(_ == _)

    // case class Person(name: Text, age: Int)
    // case class Organisation(name: Text, ceo: Person)

    // suite(t"Generic Derivation tests"):
    //   test(t"write a simple case class"):
    //     Person(t"John Smith", 65).codl
    //   .matches:
    //     case CodlDoc(_, List(
    //            Param(t"name", t"John Smith", _, _, _, _),
    //            Param(t"age", t"65", _, _, _, _)
    //          ), _) =>
      
    // //   test(t"write a nested case class"):
    // //     Organisation(t"Acme Inc", Person(t"John Smith", 65)).codl
    // //   .matches:
    // //     case CodlDoc(_, List(
    // //            Param(t"name", t"Acme Inc", _, _, _, _),
    // //            Branch(t"ceo", _, List(
    // //              Param(t"name", t"John Smith", _, _, _, _),
    // //              Param(t"age", t"65", _, _, _, _)
    // //            ), _, _)
    // //          ), _) =>

    // //   test(t"serialize a simple case class"):
    // //     Person(t"John", 65).codl.show
    // //   .oldAssert(_ == t"""|name John
    // //                       |age 65
    // //                       |""".s.stripMargin.show)
      
    // //   test(t"serialize a case class with long string"):
    // //     Person(t"John Smith", 65).codl.show
    // //   .oldAssert(_ == t"""|name
    // //                       |    John Smith
    // //                       |age 65
    // //                       |""".s.stripMargin.show)
      
    // //   test(t"serialize a nested case class"):
    // //     Organisation(t"Acme", Person(t"John", 65)).codl.show
    // //   .oldAssert(_ == t"""|name Acme
    // //                       |ceo John 65
    // //                       |""".s.stripMargin.show)

    // //   test(t"serialize a nested case class and long strings"):
    // //     Organisation(t"Acme Inc", Person(t"John Smith", 65)).codl.show
    // //   .oldAssert(_ == t"""|name
    // //                       |    Acme Inc
    // //                       |ceo
    // //                       |  name
    // //                       |      John Smith
    // //                       |  age 65
    // //                       |""".s.stripMargin.show)
    
    // // suite(t"Data roundtrip tests"):
    // //   test(t"serialize and deserialize a case class"):
    // //     val codl = Person(t"John Smith", 65).codl
    // //     Log.info(codl.show)
    // //     Codl.parse(codl.show).as[Person]
    // //   .oldAssert(_ == Person(t"John Smith", 64))