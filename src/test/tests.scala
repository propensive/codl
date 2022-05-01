package codala

import probably.*
import gossamer.*
import rudiments.*
import turbulence.*
import eucalyptus.*

given Log(Everything |-> SystemOut)

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"CoDaLa tests"):
  def run(using Runner): Unit =
    // suite(t"Parsing tests"):
    //   test(t"Parse smallest document"):
    //     Codala.parse(t"""|root
    //                     |""".s.stripMargin.show).children.head()
    //   .oldAssert(_ == t"root")
      
    //   test(t"Parse simple tree and get root"):
    //     Codala.parse(t"""|root
    //                     |  child
    //                     |""".s.stripMargin.show).children.head()
    //   .oldAssert(_ == t"root")
      
    //   test(t"Parse simple tree and get child"):
    //     Codala.parse(t"""|root
    //                     |  child
    //                     |""".s.stripMargin.show).children.head.children.head()
    //   .oldAssert(_ == t"child")
      
    //   test(t"Parse more deeply-nested child"):
    //     Codala.parse(t"""|root
    //                     |  child
    //                     |    node
    //                     |""".s.stripMargin.show).children.head.children.head.children.head()
    //   .oldAssert(_ == t"node")
      
    //   test(t"Allow prefix on first line"):
    //     Codala.parse(t"""|  root
    //                     |    child
    //                     |      node
    //                     |""".s.stripMargin.show).children.head.children.head.children.head()
    //   .oldAssert(_ == t"node")
      
    //   test(t"Allow odd prefix on first line"):
    //     Codala.parse(t"""|   root
    //                     |     child
    //                     |       node
    //                     |""".s.stripMargin.show).children.head.children.head.children.head()
    //   .oldAssert(_ == t"node")

    //   test(t"Only one space of indentation fails"):
    //     capture:
    //       Codala.parse(t"""|root
    //                       | child
    //                       |""".s.stripMargin.show)
    //   .matches:
    //     case CodalaParseError(6, CodalaParseError.Indentation.Uneven) =>
      
    //   test(t"Indentation less than initial prefix is an error"):
    //     capture:
    //       Codala.parse(t"""|    root
    //                       |      child
    //                       |   broken
    //                       |""".s.stripMargin.show)
    //   .matches:
    //     case CodalaParseError(24, CodalaParseError.Indentation.Insufficient) =>

    //   test(t"Initial prefix can be after newline"):
    //     Codala.parse(t"""
    //                   root
    //                     child
    //                     """).children.head.children.head()
    //   .oldAssert(_ == t"child")
      
    //   test(t"Last line does not need to be terminated"):
    //     Codala.parse(t"root")
    //   .oldAssert(_.children.length == 1)
      
    //   test(t"Comment is not a node"):
    //     Codala.parse(t"""
    //       #comment
    //       root
    //       """)
    //   .oldAssert(_.children.length == 1)
      
    //   test(t"Comment may be child"):
    //     Codala.parse(t"""
    //       node1
    //         #comment
    //       node2
    //       """)
    //   .oldAssert(_.children.length == 2)
      
    //   test(t"Empty string is empty document"):
    //     Codala.parse(t"")
    //   .oldAssert(_.children.length == 0)

    //   test(t"Document starts with comment"):
    //     Codala.parse(t"""|#comment
    //                     |root
    //                     |""".s.stripMargin.show)
    //   .oldAssert(_.children.length == 1)
      
    //   test(t"Unindent after comment forbidden"):
    //     capture:
    //       Codala.parse(t"""|  #comment
    //                       |root
    //                       |""".s.stripMargin.show)
    //   .matches:
    //     case CodalaParseError(12, CodalaParseError.Indentation.Insufficient) =>
      
    //   test(t"root node with params"):
    //     Codala.parse(t"root param1 param2").children.head.params.length
    //   .oldAssert(_ == 2)
      
    //   test(t"root node with children counts params correctly"):
    //     Codala.parse(t"""
    //       root param1 param2
    //         child1
    //         child2
    //     """).children.head.params
    //   .oldAssert(_.length == 2)
      
    //   test(t"root node with params counts children correctly"):
    //     Codala.parse(t"""
    //       root param1 param2
    //         child1
    //         child2
    //     """).children.head.children
    //   .oldAssert(_.length == 2)
      
    //   test(t"child node with param"):
    //     Codala.parse(t"""
    //       root param1 param2
    //         child1 param1
    //     """).children.head.children.head.params
    //   .oldAssert(_.length == 1)
      
    //   test(t"child node with param and trailing comment"):
    //     Codala.parse(t"""
    //       root param1 param2
    //         child1 param1 # line comment
    //     """).children.head.children.head.params
    //   .oldAssert(_.length == 1)
      
    //   test(t"misaligned comments"):
    //     capture:
    //       Codala.parse(t"""
    //         root param
    //           # comment 1
    //             # comment 2
    //             child1
    //       """)
    //   .oldAssert(_ == CodalaParseError(66, CodalaParseError.Indentation.AfterComment))
      
    //   test(t"comment not aligned with associated child"):
    //     capture:
    //       Codala.parse(t"""
    //         root param
    //           # comment 1
    //             child1
    //       """)
    //   .oldAssert(_ == CodalaParseError(66, CodalaParseError.Indentation.AfterComment))
      
    //   test(t"unindent on comment permitted"):
    //     Codala.parse(t"""
    //       root param
    //         child1
    //           grandchild1
    //         # unindented comment
    //     """)
    //   .oldAssert { _ => true }

    //   test(t"single unindent"):
    //     Codala.parse(t"""|root
    //                     |  child1
    //                     |    grandchild1
    //                     |  child2
    //                     |""".s.stripMargin.show).children.head.children(1)()
    //   .oldAssert(_ == t"child2")
      
    //   test(t"double unindent"):
    //     Codala.parse(t"""|root
    //                     |  child
    //                     |    grandchild
    //                     |root2
    //                     |""".s.stripMargin.show).children(1)()
    //   .oldAssert(_ == t"root2")

    //   test(t"indented param"):
    //     Codala.parse(t"""
    //       root
    //           Long text
    //         child
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Long text")
      
    //   test(t"multiline param"):
    //     Codala.parse(t"""
    //       root
    //           Line 1
    //           Line 2
    //         child
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Line 1\nLine 2")
      
    //   test(t"multiline param allows uneven indentation"):
    //     Codala.parse(t"""
    //       root
    //           Line 1
    //            Line 2
    //         child
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Line 1\n Line 2")
      
    //   test(t"indented param can be last thing in doc"):
    //     Codala.parse(t"""
    //       root
    //           Long text
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Long text")
      
    //   test(t"multiline param includes trailing spaces"):
    //     Codala.parse(t"""
    //       root
    //           Long text   
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Long text   ")
      
    //   test(t"multiline param excludes trailing newline"):
    //     Codala.parse(t"""
    //       root
    //           Long text

    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"Long text")
      
    //   test(t"multiline param is not a comment"):
    //     Codala.parse(t"""
    //       root
    //           # Long text
    //     """).children.head.params.head.value
    //   .oldAssert(_ == t"# Long text")
      
    //   test(t"trailing comment is not a param"):
    //     Codala.parse(t"""
    //       root
    //         child abc # comment
    //     """).children.head.children.head.params
    //   .oldAssert(_.length == 1)
      
    //   test(t"trailing comment is accessible"):
    //     Codala.parse(t"""
    //       root
    //         child abc # comment
    //     """).children.head.children.head match
    //       case Node.Data(_, _, _, c, _) => c
    //   .oldAssert(_ == Some(t"comment"))
      
    //   test(t"leading comment is accessible"):
    //     Codala.parse(t"""
    //       root
    //         # message
    //         child abc
    //     """).children.head.children.head.comment
    //   .oldAssert(_ == Some(t"message"))
      
    //   test(t"leading comment is two lines long"):
    //     Codala.parse(t"""
    //       root
    //         # line 1
    //         # line 2
    //         child abc
    //     """).children.head.children.head.comment
    //   .oldAssert(_ == Some(t"line 1\nline 2"))
      
    //   test(t"blank line separates comments"):
    //     Codala.parse(t"""
    //       root
    //         # line 1

    //         # line 2
    //         child abc
    //     """).children.head.children(1).comment
    //   .oldAssert(_ == Some(t"line 2"))
      
    //   test(t"standalone comment is accessible"):
    //     Codala.parse(t"""
    //       root
    //         # line 1

    //         # line 2
    //         child abc
    //     """).children.head.children.head.comment
    //   .oldAssert(_ == Some(t"line 1"))
      
    //   test(t"standalone comment is accessible"):
    //     Codala.parse(t"""
    //       root
    //         # line 1

    //         # line 2
    //         child abc
    //     """).children.head.children.head.comment
    //   .oldAssert(_ == Some(t"line 1"))
      
    //   test(t"blank lines are counted"):
    //     Codala.parse(t"""
    //       root


    //         child abc
    //     """).children.head.children.head match
    //       case blank@Node.Blank(_, _, _) => blank
    //   .oldAssert(_.length == 2)

    // suite(t"Schema tests"):

    //   val basicSchema = Schema.parse(Codala.parse(t"""
    //     root
    //       item
    //         value?
    //         element? param
    //         option* id!
    //   """))

    //   test(t"Simple schema structure"):
    //     basicSchema.validate(Codala.parse(t"""
    //       root
    //         item
    //           value
    //     """))
    //   .oldAssert(_ => true)
      
    //   test(t"Schema doesn't contain child"):
    //     capture:
    //       basicSchema.validate(Codala.parse(t"""
    //         root
    //           child
    //       """))
    //   .matches:
    //     case CodalaValidationError(_, _, CodalaValidationError.Issue.InvalidKey(t"child")) =>
        
    //   test(t"required value must be included"):
    //     capture:
    //       basicSchema.validate(Codala.parse(t"""
    //         root
    //       """))
    //   .matches:
    //     case CodalaValidationError(_, _, CodalaValidationError.Issue.MissingKey(t"item")) =>
        
    //   test(t"unique value should not be duplicated"):
    //     capture:
    //       basicSchema.validate(Codala.parse(t"""
    //         root
    //           item
    //             value
    //             value
    //       """))
    //   .matches:
    //     case CodalaValidationError(_, _, CodalaValidationError.Issue.DuplicateKey(t"value", _)) =>
      
    //   test(t"unique value with param should not be duplicated"):
    //     capture:
    //       basicSchema.validate(Codala.parse(t"""
    //         root
    //           item
    //             element one
    //             element two
    //       """))
    //   .matches:
    //     case CodalaValidationError(_, _, CodalaValidationError.Issue.DuplicateKey(t"element", _)) =>

    //   test(t"required param is missing"):
    //     capture:
    //       basicSchema.validate(Codala.parse(t"""
    //         root
    //           item
    //             element
    //       """))
    //   .matches:
    //     case CodalaValidationError(_, _, CodalaValidationError.Issue.MissingParams(1)) =>

    //   test(t"too many parameters"):
    //     capture:
    //       basicSchema.validate(Codala.parse(t"""
    //         root
    //           item
    //             element one two
    //       """))
    //   .matches:
    //     case CodalaValidationError(_, _, CodalaValidationError.Issue.SurplusParams(1)) =>
      
    //   test(t"duplicated ID"):
    //     capture:
    //       basicSchema.validate(Codala.parse(t"""
    //         root
    //           item
    //             option one
    //             option one
    //       """))
    //   .matches:
    //     case CodalaValidationError(_, _, CodalaValidationError.Issue.DuplicateId(t"one", 60)) =>
      
    suite(t"Binary tests"):

      def roundtrip(doc: ValidDoc): ValidDoc = ValidDoc.read(doc.schema, doc.bin)

      val basicSchema = Schema.parse(Codala.parse(t"""
        root
          child?
          item
            value?
            element? arg
            option* id!
      """))

      test(t"simple structure"):
        val validDoc = basicSchema.validate(Codala.parse(t"""
          root
            child
            item
              value
              element abc
        """))
        
        validDoc.data -> roundtrip(validDoc).data
      .oldAssert(_ == _)
