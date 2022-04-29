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

    test(t"Parse smallest document"):
      Codala.parse("""|root
                      |""".stripMargin).children.head()
    .oldAssert(_ == "root")
    
    test(t"Parse simple tree and get root"):
      Codala.parse("""|root
                      |  child
                      |""".stripMargin).children.head()
    .oldAssert(_ == "root")
    
    test(t"Parse simple tree and get child"):
      Codala.parse("""|root
                      |  child
                      |""".stripMargin).children.head.children.head()
    .oldAssert(_ == "child")
    
    test(t"Parse more deeply-nested child"):
      Codala.parse("""|root
                      |  child
                      |    node
                      |""".stripMargin).children.head.children.head.children.head()
    .oldAssert(_ == "node")
    
    test(t"Allow prefix on first line"):
      Codala.parse("""|  root
                      |    child
                      |      node
                      |""".stripMargin).children.head.children.head.children.head()
    .oldAssert(_ == "node")
    
    test(t"Allow odd prefix on first line"):
      Codala.parse("""|   root
                      |     child
                      |       node
                      |""".stripMargin).children.head.children.head.children.head()
    .oldAssert(_ == "node")

    test(t"Only one space of indentation fails"):
      capture:
        Codala.parse("""|root
                        | child
                        |""".stripMargin)
    .matches:
      case UnevenIndentationError(6) =>
    
    test(t"Indentation less than initial prefix is an error"):
      capture:
        Codala.parse("""|    root
                        |      child
                        |   broken
                        |""".stripMargin)
    .matches:
      case NotEnoughIndentationError(24) =>

    test(t"Initial prefix can be after newline"):
      Codala.parse("""
                   root
                     child
                     """).children.head.children.head()
    .oldAssert(_ == "child")
    
    test(t"Last line does not need to be terminated"):
      Codala.parse("""root""")
    .oldAssert(_.children.length == 1)
    
    test(t"Comment is not a node"):
      Codala.parse("""
        #comment
        root
        """)
    .oldAssert(_.children.length == 1)
    
    test(t"Comment may be child"):
      Codala.parse("""
        node1
          #comment
        node2
        """)
    .oldAssert(_.children.length == 2)
    
    test(t"Empty string is empty document"):
      Codala.parse("")
    .oldAssert(_.children.length == 0)

    test(t"Document starts with comment"):
      Codala.parse("""|#comment
                      |root
                      |""".stripMargin)
    .oldAssert(_.children.length == 1)
    
    test(t"Unindent after comment forbidden"):
      capture:
        Codala.parse("""|  #comment
                        |root
                        |""".stripMargin)
    .matches:
      case NotEnoughIndentationError(12) =>
    
    test(t"root node with params"):
      Codala.parse("root param1 param2").children.head.params.length
    .oldAssert(_ == 2)
    
    test(t"root node with children counts params correctly"):
      Codala.parse("""
        root param1 param2
          child1
          child2
      """).children.head.params
    .oldAssert(_.length == 2)
    
    test(t"root node with params counts children correctly"):
      Codala.parse("""
        root param1 param2
          child1
          child2
      """).children.head.children
    .oldAssert(_.length == 2)
    
    test(t"child node with param"):
      Codala.parse("""
        root param1 param2
          child1 param1
      """).children.head.children.head.params
    .oldAssert(_.length == 1)
    
    test(t"child node with param and trailing comment"):
      Codala.parse("""
        root param1 param2
          child1 param1 # line comment
      """).children.head.children.head.params
    .oldAssert(_.length == 1)
    
    test(t"misaligned comments"):
      capture:
        Codala.parse("""
          root param
            # comment 1
              # comment 2
              child1
        """)
    .oldAssert(_ == IndentationAfterCommentError(60))
    
    test(t"comment not aligned with associated child"):
      capture:
        Codala.parse("""
          root param
            # comment 1
              child1
        """)
    .oldAssert(_ == IndentationAfterCommentError(60))
    
    test(t"unindent on comment permitted"):
      Codala.parse("""
        root param
          child1
            grandchild1
          # unindented comment
      """)
    .oldAssert { _ => true }

    test(t"single unindent"):
      Codala.parse("""|root
                      |  child1
                      |    grandchild1
                      |  child2
                      |""".stripMargin).children.head.children(1)()
    .oldAssert(_ == "child2")
    
    test(t"double unindent"):
      Codala.parse("""|root
                      |  child
                      |    grandchild
                      |root2
                      |""".stripMargin).children(1)()
    .oldAssert(_ == "root2")

    test(t"indented param"):
      Codala.parse("""
        root
            Long text
          child
      """).children.head.params.head.value
    .oldAssert(_ == "Long text")
    
    test(t"multiline param"):
      Codala.parse("""
        root
            Line 1
            Line 2
          child
      """).children.head.params.head.value
    .oldAssert(_ == "Line 1\nLine 2")
    
    test(t"multiline param allows uneven indentation"):
      Codala.parse("""
        root
            Line 1
             Line 2
          child
      """).children.head.params.head.value
    .oldAssert(_ == "Line 1\n Line 2")
    
    test(t"indented param can be last thing in doc"):
      Codala.parse("""
        root
            Long text
      """).children.head.params.head.value
    .oldAssert(_ == "Long text")
    
    test(t"multiline param includes trailing spaces"):
      Codala.parse("""
        root
            Long text   
      """).children.head.params.head.value
    .oldAssert(_ == "Long text   ")
    
    test(t"multiline param excludes trailing newline"):
      Codala.parse("""
        root
            Long text

      """).children.head.params.head.value
    .oldAssert(_ == "Long text")
    
    test(t"multiline param is not a comment"):
      Codala.parse("""
        root
            # Long text
      """).children.head.params.head.value
    .oldAssert(_ == "# Long text")
    
    test(t"trailing comment is not a param"):
      Codala.parse("""
        root
          child abc # comment
      """).children.head.children.head.params
    .oldAssert(_.length == 1)
    
    test(t"trailing comment is accessible"):
      Codala.parse("""
        root
          child abc # comment
      """).children.head.children.head match
        case Node.Data(_, _, _, c, _) => c
    .oldAssert(_ == Some("comment"))
    
    test(t"leading comment is accessible"):
      Codala.parse("""
        root
          # message
          child abc
      """).children.head.children.head.comment
    .oldAssert(_ == Some("message"))
    
    test(t"leading comment is two lines long"):
      Codala.parse("""
        root
          # line 1
          # line 2
          child abc
      """).children.head.children.head.comment
    .oldAssert(_ == Some("line 1\nline 2"))
    
    test(t"blank line separates comments"):
      Codala.parse("""
        root
          # line 1

          # line 2
          child abc
      """).children.head.children.head.comment
    .oldAssert(_ == Some("line 2"))