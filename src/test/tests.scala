package codl

import probably.*
import gossamer.*
import rudiments.*
import turbulence.*
import eucalyptus.*

given Log(Everything |-> SystemOut)

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"CoDL tests"):
  def run(using Runner): Unit =
    suite(t"Parsing tests"):
      test(t"Parse smallest document"):
        Codl.parse(t"""|root
                        |""".s.stripMargin.show).children.head()
      .oldAssert(_ == t"root")
      
      test(t"Parse simple tree and get root"):
        Codl.parse(t"""|root
                        |  child
                        |""".s.stripMargin.show).children.head()
      .oldAssert(_ == t"root")
      
      test(t"Parse simple tree and get child"):
        Codl.parse(t"""|root
                        |  child
                        |""".s.stripMargin.show).children.head.children.head()
      .oldAssert(_ == t"child")
      
      test(t"Parse more deeply-nested child"):
        Codl.parse(t"""|root
                        |  child
                        |    node
                        |""".s.stripMargin.show).children.head.children.head.children.head()
      .oldAssert(_ == t"node")
      
      test(t"Allow prefix on first line"):
        Codl.parse(t"""|  root
                        |    child
                        |      node
                        |""".s.stripMargin.show).children.head.children.head.children.head()
      .oldAssert(_ == t"node")
      
      test(t"Allow odd prefix on first line"):
        Codl.parse(t"""|   root
                        |     child
                        |       node
                        |""".s.stripMargin.show).children.head.children.head.children.head()
      .oldAssert(_ == t"node")

      test(t"Only one space of indentation fails"):
        capture:
          Codl.parse(t"""|root
                          | child
                          |""".s.stripMargin.show)
      .matches:
        case CodlParseError(6, CodlParseError.Indentation.Uneven) =>
      
      test(t"Indentation less than initial prefix is an error"):
        capture:
          Codl.parse(t"""|    root
                          |      child
                          |   broken
                          |""".s.stripMargin.show)
      .matches:
        case CodlParseError(24, CodlParseError.Indentation.Insufficient) =>

      test(t"Initial prefix can be after newline"):
        Codl.parse(t"""
                      root
                        child
                        """).children.head.children.head()
      .oldAssert(_ == t"child")
      
      test(t"Last line does not need to be terminated"):
        Codl.parse(t"root")
      .oldAssert(_.children.length == 1)
      
      test(t"Comment is not a node"):
        Codl.parse(t"""
          #comment
          root
          """)
      .oldAssert(_.children.length == 1)
      
      test(t"Comment may be child"):
        Codl.parse(t"""
          node1
            #comment
          node2
          """)
      .oldAssert(_.children.length == 2)
      
      test(t"Empty string is empty document"):
        Codl.parse(t"")
      .oldAssert(_.children.length == 0)

      test(t"Document starts with comment"):
        Codl.parse(t"""|#comment
                        |root
                        |""".s.stripMargin.show)
      .oldAssert(_.children.length == 1)
      
      test(t"Unindent after comment forbidden"):
        capture:
          Codl.parse(t"""|  #comment
                          |root
                          |""".s.stripMargin.show)
      .matches:
        case CodlParseError(12, CodlParseError.Indentation.Insufficient) =>
      
      test(t"root node with params"):
        Codl.parse(t"root param1 param2").children.head.params.length
      .oldAssert(_ == 2)
      
      test(t"root node with children counts params correctly"):
        Codl.parse(t"""
          root param1 param2
            child1
            child2
        """).children.head.params
      .oldAssert(_.length == 2)
      
      test(t"root node with params counts children correctly"):
        Codl.parse(t"""
          root param1 param2
            child1
            child2
        """).children.head.children
      .oldAssert(_.length == 2)
      
      test(t"child node with param"):
        Codl.parse(t"""
          root param1 param2
            child1 param1
        """).children.head.children.head.params
      .oldAssert(_.length == 1)
      
      test(t"child node with param and trailing comment"):
        Codl.parse(t"""
          root param1 param2
            child1 param1 # line comment
        """).children.head.children.head.params
      .oldAssert(_.length == 1)
      
      test(t"misaligned comments"):
        capture:
          Codl.parse(t"""
            root param
              # comment 1
                # comment 2
                child1
          """)
      .oldAssert(_ == CodlParseError(66, CodlParseError.Indentation.AfterComment))
      
      test(t"comment not aligned with associated child"):
        capture:
          Codl.parse(t"""
            root param
              # comment 1
                child1
          """)
      .oldAssert(_ == CodlParseError(66, CodlParseError.Indentation.AfterComment))
      
      test(t"unindent on comment permitted"):
        Codl.parse(t"""
          root param
            child1
              grandchild1
            # unindented comment
        """)
      .oldAssert { _ => true }

      test(t"single unindent"):
        Codl.parse(t"""|root
                        |  child1
                        |    grandchild1
                        |  child2
                        |""".s.stripMargin.show).children.head.children(1)()
      .oldAssert(_ == t"child2")
      
      test(t"double unindent"):
        Codl.parse(t"""|root
                        |  child
                        |    grandchild
                        |root2
                        |""".s.stripMargin.show).children(1)()
      .oldAssert(_ == t"root2")

      test(t"indented param"):
        Codl.parse(t"""
          root
              Long text
            child
        """).children.head.params.head.value
      .oldAssert(_ == t"Long text")
      
      test(t"multiline param"):
        Codl.parse(t"""
          root
              Line 1
              Line 2
            child
        """).children.head.params.head.value
      .oldAssert(_ == t"Line 1\nLine 2")
      
      test(t"multiline param allows uneven indentation"):
        Codl.parse(t"""
          root
              Line 1
               Line 2
            child
        """).children.head.params.head.value
      .oldAssert(_ == t"Line 1\n Line 2")
      
      test(t"indented param can be last thing in doc"):
        Codl.parse(t"""
          root
              Long text
        """).children.head.params.head.value
      .oldAssert(_ == t"Long text")
      
      test(t"multiline param includes trailing spaces"):
        Codl.parse(t"""
          root
              Long text   
        """).children.head.params.head.value
      .oldAssert(_ == t"Long text   ")
      
      test(t"multiline param excludes trailing newline"):
        Codl.parse(t"""
          root
              Long text

        """).children.head.params.head.value
      .oldAssert(_ == t"Long text")
      
      test(t"multiline param is not a comment"):
        Codl.parse(t"""
          root
              # Long text
        """).children.head.params.head.value
      .oldAssert(_ == t"# Long text")
      
      test(t"trailing comment is not a param"):
        Codl.parse(t"""
          root
            child abc # comment
        """).children.head.children.head.params
      .oldAssert(_.length == 1)
      
      test(t"trailing comment is accessible"):
        Codl.parse(t"""
          root
            child abc # comment
        """).children.head.children.head
      .matches:
        case Node.Data(_, _, _, Some(t"comment"), _) =>
      
      test(t"leading comment is accessible"):
        Codl.parse(t"""
          root
            # message
            child abc
        """).children.head.children.head.comment
      .oldAssert(_ == Some(t" message"))
      
      test(t"leading comment is two lines long"):
        Codl.parse(t"""
          root
            # line 1
            # line 2
            child abc
        """).children.head.children.head.comment
      .oldAssert(_ == Some(t" line 1\n line 2"))
      
      test(t"blank line separates comments"):
        Codl.parse(t"""
          root
            # line 1

            # line 2
            child abc
        """).children.head.children(1).comment
      .oldAssert(_ == Some(t" line 2"))
      
      test(t"standalone comment is accessible"):
        Codl.parse(t"""
          root
            # line 1

            # line 2
            child abc
        """).children.head.children.head.comment
      .oldAssert(_ == Some(t" line 1"))
      
      test(t"blank lines are counted"):
        Codl.parse(t"""
          root


            child abc
        """).children.head.children.head
      .matches:
        case Node.Blank(_, 2, _) =>

    suite(t"Schema tests"):

      val basicSchema = Schema.parse(Codl.parse(t"""
        root
          item
            value?
            element? param
            option* id!
      """))

      test(t"Simple schema structure"):
        basicSchema.validate(Codl.parse(t"""
          root
            item
              value
        """))
      .oldAssert(_ => true)
      
      test(t"Schema doesn't contain child"):
        capture:
          basicSchema.validate(Codl.parse(t"""
            root
              child
          """))
      .matches:
        case CodlValidationError(_, _, CodlValidationError.Issue.InvalidKey(t"child")) =>
        
      test(t"required value must be included"):
        capture:
          basicSchema.validate(Codl.parse(t"""
            root
          """))
      .matches:
        case CodlValidationError(_, _, CodlValidationError.Issue.MissingKey(t"item")) =>
        
      test(t"unique value should not be duplicated"):
        capture:
          basicSchema.validate(Codl.parse(t"""
            root
              item
                value
                value
          """))
      .matches:
        case CodlValidationError(_, _, CodlValidationError.Issue.DuplicateKey(t"value", _)) =>
      
      test(t"unique value with param should not be duplicated"):
        capture:
          basicSchema.validate(Codl.parse(t"""
            root
              item
                element one
                element two
          """))
      .matches:
        case CodlValidationError(_, _, CodlValidationError.Issue.DuplicateKey(t"element", _)) =>

      test(t"required param is missing"):
        capture:
          basicSchema.validate(Codl.parse(t"""
            root
              item
                element
          """))
      .matches:
        case CodlValidationError(_, _, CodlValidationError.Issue.MissingParams(1)) =>

      test(t"too many parameters"):
        capture:
          basicSchema.validate(Codl.parse(t"""
            root
              item
                element one two
          """))
      .matches:
        case CodlValidationError(_, _, CodlValidationError.Issue.SurplusParams(1)) =>
      
      test(t"duplicated ID"):
        capture:
          basicSchema.validate(Codl.parse(t"""
            root
              item
                option one
                option one
          """))
      .matches:
        case CodlValidationError(_, _, CodlValidationError.Issue.DuplicateId(t"one", 60)) =>
      
    val basicSchema = Schema.parse(Codl.parse(t"""
      root
        child?
        item
          value?
          element? arg
          option* id!
    """))
    
    suite(t"Binary tests"):

      def roundtrip(doc: CodlDoc): CodlDoc = CodlDoc.read(doc.schema, doc.bin)

      test(t"simple structure"):
        val validDoc = basicSchema.validate(Codl.parse(t"""
          root
            child
            item
              value
              element abc
        """))
      
      test(t"serialize simple structure"):
        val validDoc = basicSchema.validate(Codl.parse(t"""
          root
            child
            item
              value
              element abc
        """))
        
        validDoc.bin
      .oldAssert(_ == t"""±ÀÚ!  "   ! "   ! ! #abc""")

    suite(t"Serialization tests"):

      def compare(schema: Schema, src: Text): (Text, Text) =
        src.sub(t" ", t"_") -> schema.validate(Codl.parse(src)).show.sub(t" ", t"_")

      test(t"Simple node with child"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)
      
      test(t"Simple node with parameter"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |    element argument
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)
      
      test(t"Simple node with indentation"):
        compare(basicSchema, t"""|   root
                                 |     item
                                 |       element argument
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)
      
      test(t"Simple node with padded parameter"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |    element   argument
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)
      
      test(t"Simple node with trailing comment"):
        compare(basicSchema, t"""|root
                                 |  item # comment
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)
      
      test(t"Simple node with trailing comment and extra space"):
        compare(basicSchema, t"""|root
                                 |  item    # comment
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)

      test(t"Simple node with long parameter"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |    element
                                 |        This is some text.
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)

      test(t"Simple node with inital comment"):
        compare(basicSchema, t"""|#!/bin/bash
                                 |root
                                 |  item
                                 |    element
                                 |        This is some text.
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)

      test(t"Simple node with blank lines"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |
                                 |    element
                                 |        This is some text.
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)
      
      test(t"Serialize more complex structure"):
        compare(basicSchema, t"""|root
                                 |  item
                                 |    value
                                 |    # here's a comment
                                 |    element abc
                                 |    option xyz
                                 |  child
                                 |""".s.stripMargin.show)
      .oldAssert(_ == _)

    suite(t"Generic Derivation tests"):

      case class Person(name: Text, age: Int)
      case class Organisation(name: Text, ceo: Person)

      test(t"write a simple case class"):
        Person(t"John Smith", 65).codl
      .matches:
        case CodlDoc(_, List(
               Param(t"name", t"John Smith", _, _, _, _),
               Param(t"age", t"65", _, _, _, _)
             ), _) =>
      
      test(t"write a nested case class"):
        Organisation(t"Acme Inc", Person(t"John Smith", 65)).codl
      .matches:
        case CodlDoc(_, List(
               Param(t"name", t"Acme Inc", _, _, _, _),
               Branch(t"ceo", _, List(
                 Param(t"name", t"John Smith", _, _, _, _),
                 Param(t"age", t"65", _, _, _, _)
               ), _, _)
             ), _) =>

      test(t"serialize a simple case class"):
        Person(t"John", 65).codl.show
      .oldAssert(_ == t"""|name John
                          |age 65
                          |""".s.stripMargin.show)
      
      test(t"serialize a case class with long string"):
        Person(t"John Smith", 65).codl.show
      .oldAssert(_ == t"""|name
                          |    John Smith
                          |age 65
                          |""".s.stripMargin.show)
      
      test(t"serialize a nested case class"):
        Organisation(t"Acme", Person(t"John", 65)).codl.show
      .oldAssert(_ == t"""|name Acme
                          |ceo John 65
                          |""".s.stripMargin.show)
      test(t"serialize a nested case class and long strings"):
        Organisation(t"Acme Inc", Person(t"John Smith", 65)).codl.show
      .oldAssert(_ == t"""|name
                          |    Acme Inc
                          |ceo
                          |  name
                          |      John Smith
                          |  age 65
                          |""".s.stripMargin.show)