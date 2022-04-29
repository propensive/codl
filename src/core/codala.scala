package codala

import java.io.*
import scala.io.*
import annotation.tailrec
import language.dynamics
import gossamer.*
import rudiments.*
import eucalyptus.*

given Realm(t"codala")

object Binary:
  def write(number: Int, out: Writer): Unit = out.write((number + 32).toChar)

  // def write(nodes: List[Node], out: Writer): Unit =
  //   out.write('\u00b1')
  //   out.write('\u00c0')
  //   out.write('\u00da')
  //   write(nodes.length, out)
  //   nodes.foreach(_.write(out))
  //   out.close()

  // def read(in: Reader): List[Node] throws BinaryError =
  //   if in.read != 177 then throw BinaryError("0xb1", 0)
  //   if in.read != 192 then throw BinaryError("0xc0", 1)
  //   if in.read != 218 then throw BinaryError("0xda", 2)
  //   (0 until readNumber(in)).map(_ => Node.read(in)).to(List)

  def readNumber(in: Reader): Int = in.read - 32

object Token:
  def apply(value: String): Token = Token(0, value, 0)
  
  def read(in: Reader): Token =
    val buf = new Array[Char](Binary.readNumber(in))
    in.read(buf)
    Token(0, String(buf), Binary.readNumber(in))

case class Token(start: Int, value: String, padding: Int = 0):
  def end: Int = start + value.length
  override def toString(): String = value+(" "*padding)

  def write(out: Writer): Unit =
    Binary.write(value.length, out)
    out.write(value)
    Binary.write(padding, out)

object Node
  // def read(in: Reader): Node =
  //   val key = Token(keys(Binary.readNumber(in)))
  //   val values = (0 until Binary.readNumber(in)).map(_ => Token.read(in))
  //   val children = (0 until Binary.readNumber(in)).map(_ => Node.read(in))
  //   Node(key, values.to(List), children.to(List))

enum Node:
  case Doc(childNodes: List[Data | Blank])
  case Blank(length: Int, leadingComment: Option[String])
  case Data(key: Token, args: List[Token], childNodes: List[Data | Blank], trailingComment: Option[String] = None, leadingComment: Option[String])
  // def write(out: Writer): Unit =
  //   Binary.write(keyMap(key.value), out)
  //   Binary.write(params.length, out)
  //   params.foreach(_.write(out))
  //   Binary.write(children.length, out)
  //   children.foreach(_.write(out))

  def children: List[Data | Blank] = this match
    case Doc(childNodes) => childNodes
    case Blank(_, _) => Nil
    case Data(_, _, childNodes, _, _) => childNodes

  def params: List[Token] = this match
    case Doc(childNodes) => Nil
    case Blank(_, _) => Nil
    case Data(_, params, _, _, _) => params

  def comment: Option[String] = this match
    case Doc(_) => None
    case Blank(_, c) => c
    case Data(_, _, _, _, c) => c

  def apply(): String = this match
    case Data(key, params, children, comment, leadingComment) => (key :: params).map(_.value).mkString(" ")
    case Doc(children) => ""
    case Blank(rows, comment) => comment.getOrElse("")

  def render(indent: Int = 0): Unit = this match
    case Blank(rows, comment) => println()
    case Doc(children) => children.foreach(_.render(0))
    case Data(key, params, children, comment, leadingComment) =>
      print(" "*(indent*2))
      print(key.value)
      print(" ")
      println(params.map(_.toString).mkString(" "))
      children.foreach(_.render(indent + 1))

case class Comment(pos: Int, indent: Int, value: String)

object Codala:
  def parse(string: String)(using Log): Node.Doc throws ParseError = parse(StringReader(string))

  def parse(reader: Reader)(using Log): Node.Doc throws ParseError =
    var pos = 0
    var char: Int = reader.read()
    var initialPrefix = 0
    val buffer: StringBuilder = StringBuilder()
    var comment: Boolean = false
    var commentOpt: Option[String] = None
    var multiline: Boolean = false
    var stack: List[Node.Data] = Nil
    var node: Node.Data = Node.Data(Token("ROOT"), Nil, Nil, None, None)
    var currentIndent: Int = 0
    def continue = char != -1 && char != '\n'
    
    def next(): Boolean =
      if char != -1 then
        char = reader.read()
        pos += 1
        continue
      else false
    
    def skip(): Int =
      val start = pos
      while continue && char == ' ' do next()
      pos - start
    
    def cue(limit: Int): Boolean =
      while continue && pos < limit && next() && char == ' ' do ()
      pos == limit
    
    def token(): Token =
      buffer.clear()
      val start = pos
      while
        buffer.append(char.toChar)
        continue && next() && char != ' ' && char != '\n'
      do ()
      val token = Token(start, buffer.toString, skip())
      buffer.clear()
      token
    
    def line(): Unit =
      while
        buffer.append(char.toChar)
        continue && next() && char != '\n'
      do ()

    def pop(): Unit =
      val lastNode = node.copy(args = node.args.reverse, childNodes = node.children.reverse)
      node = stack.head
      node = node.copy(childNodes = lastNode :: node.children)
      stack = stack.tail
      currentIndent -= 1
    
    def param(token: Token): Unit =
      if token.value == "#" then
        comment = true
        line()
        node = node.copy(trailingComment = Some(buffer.toString))
        buffer.clear()
      else node = node.copy(args = token :: node.args)

    while
      initialPrefix = skip()
      char == '\n'
    do next()

    while char != -1 do
      val count = skip()
      val indent = count/2

      if char == '\n' then
        if comment then
          buffer.clear()
          comment = false
      else if currentIndent + 1 == indent then
        if comment then throw IndentationAfterCommentError(pos)
        if multiline then
          buffer.append('\n')
          for i <- (currentIndent + 1)*2 until count do buffer.append(' ')
        else if indent > currentIndent + 2 then throw TooMuchIndentationError(pos)
        multiline = true
        line()
      else if char == '#' then
        next()
        skip()
        if comment then buffer.append('\n') else comment = true
        line()
      else if count%2 == 1 && !multiline then throw UnevenIndentationError(pos)
      else
        if comment then
          commentOpt = Some(buffer.toString)
          buffer.clear()
          comment = false
        
        if multiline then
          if !buffer.isEmpty then param(Token(buffer.toString))
          buffer.clear()
          multiline = false
        
        while indent < currentIndent do pop()
        val tok = token()
        stack ::= node
        
        if multiline && !buffer.isEmpty then
          param(token())
          buffer.clear()
          multiline = false
        
        node = Node.Data(tok, Nil, Nil, None, commentOpt)
        commentOpt = None
        currentIndent += 1
        while continue do param(token())
      
      next()
      
      if continue && !cue(pos + initialPrefix) && char != '\n' && continue
      then throw NotEnoughIndentationError(pos)

    if multiline then
      if !buffer.isEmpty then param(Token(buffer.toString))

    while stack.length > 0 do pop()
    
    Node.Doc(node.children.reverse)

trait Serializer[T]:
  def apply(value: T): Struct

trait Deserializer[T]:
  def apply(struct: Struct): Option[T]

object Schema:
  def parse(nodes: List[Node.Data]): List[Schema] throws MultipleIdentifiersError =
    nodes.map:
      node =>
        val repeated = node.key.value.endsWith("*") || node.key.value.endsWith("+")
        val optional = node.key.value.endsWith("?") || node.key.value.endsWith("*")
        val key = if repeated || optional then node.key.value.dropRight(1) else node.key.value
        
        val id = node.params.zipWithIndex.filter(_(0).value.endsWith("!")) match
          case List((id, n)) => println("Found id "+n); Some(n)
          case Nil           => None
          case _             => throw MultipleIdentifiersError(key, node.key.start)
        
        // FIXME: Checks whether last param is id, not first
        node.params.reverse match
          case Nil =>
            Schema(key, None, !optional, repeated, Nil, parse(node.children.sift[Node.Data]))
          
          case param :: rest =>
            val variadic = param.value.endsWith("*") || param.value.endsWith("+")
            val skippable = param.value.endsWith("?") || param.value.endsWith("*")
            val drop = if variadic || skippable || param.value.endsWith("!") then 1 else 0
            val params = param.value.dropRight(drop) :: rest.map(_.value)
            
            Schema(key, id, !optional, repeated, params.reverse, parse(node.children.sift[Node.Data]),
                variadic = variadic, allRequired = !skippable)

class ParseError(msg: String) extends Exception(msg)

case class UnevenIndentationError(pos: Int)
extends ParseError(s"expected an even number of indentation spaces at position $pos")

case class IndentationAfterCommentError(pos: Int)
extends ParseError(s"indentation appears after a comment at $pos")

case class TooMuchIndentationError(pos: Int)
extends ParseError(s"too much indentation at $pos")

case class NotEnoughIndentationError(pos: Int)
extends ParseError(s"not enough indentation at position $pos")

case class BinaryError(expectation: String, pos: Int)
extends Exception(s"expected $expectation at position $pos")

case class MissingParamsError(key: String, minimum: Int, pos: Int)
extends Exception(s"the key $key requires at least $minimum parameter(s) at position $pos")

case class MissingKeyError(key: String, parent: String, pos: Int)
extends Exception(s"the key $key is required inside $parent at position $pos")

case class InvalidKeyError(key: String, parent: String, pos: Int)
extends Exception(s"the key $key is not valid inside $parent at position $pos")

case class DuplicateKeyError(key: String, pos: Int, firstPos: Int)
extends Exception(s"the unique key $key first used at $firstPos is duplicated at $pos")

case class SurplusParamsError(key: String, maximum: Int, pos: Int)
extends Exception(s"the key $key may have at most $maximum parameters(s) at position $pos")

case class MultipleIdentifiersError(key: String, pos: Int)
extends Exception(s"multiple parameters of $key have been marked as identifiers at position $pos")

case class DuplicateIdentifierError(id: String, firstPos: Int, pos: Int)
extends Exception(s"the id $id is duplicated in positions $firstPos and $pos")

case class Struct(key: Token, children: List[Struct]) extends Dynamic:
  def apply(): String = children.head.key.value
  def as[T](using deserializer: Deserializer[T]): T = deserializer(this).get
  def selectDynamic(key: String): Struct = children.find(_.key.value == key).get
  def applyDynamic(key: String)(): String = selectDynamic(key).apply()
  
  def render(indent: Int = 0): Unit =
    print("  "*indent)
    println(key.value)
    children.foreach(_.render(indent + 1))


case class Schema(key: String, identifier: Option[Int], required: Boolean, repeatable: Boolean,
                      params: List[String] = Nil, children: List[Schema] = Nil,
                      allRequired: Boolean = true, variadic: Boolean = false):
  def apply(token: Token): Schema throws InvalidKeyError =
    children.find(_.key == token.value).getOrElse:
      throw InvalidKeyError(token.value, key, token.start)
  
  def validate(nodes: List[Node.Data]): List[Struct] throws InvalidKeyError | DuplicateKeyError |
      MissingKeyError | SurplusParamsError | MissingParamsError | DuplicateIdentifierError =
    nodes.foldLeft(Map[String, Token]()):
      case (map, node) =>
        if !apply(node.key).repeatable && map.contains(node.key.value)
        then throw DuplicateKeyError(node.key.value, node.key.start, map(node.key.value).start)
        else map.updated(node.key.value, node.key)

    val requiredKeys = nodes.map(_.key.value).to(Set)
    children.filter(_.required).foreach:
      schema =>
        if !requiredKeys.contains(schema.key)
        then throw MissingKeyError(schema.key, key, nodes.last.key.end)

    nodes.foldLeft(Map[String, Token]()):
      case (map, node) =>
        apply(node.key).identifier.map(node.params(_)).fold(map):
          ident =>
            if map.contains(ident.value)
            then throw DuplicateIdentifierError(ident.value, map(ident.value).start, ident.start)
            else map.updated(ident.value, ident)

    for node <- nodes yield
      val schema = apply(node.key)
      val minimum = if schema.allRequired then schema.params.length else schema.params.length - 1
      
      if node.params.length < minimum
      then throw MissingParamsError(node.key.value, minimum, node.key.end)
      
      if !schema.variadic && node.params.length > schema.params.length
      then throw SurplusParamsError(node.key.value, schema.params.length, node.key.start)

      def recur(schemaParams: List[String], params: List[Token], list: List[Struct] = Nil)
               : List[Struct] =
        schemaParams match
          case Nil =>
            list.reverse
          case head :: Nil =>
            Struct(Token(head), params.map(Struct(_, Nil))) :: list
          case head :: tail =>
            recur(tail, params.tail, Struct(Token(head), List(Struct(params.head, Nil))) :: list)

      val paramStructs = recur(schema.params, node.params)
      
      Struct(node.key, paramStructs ::: schema.validate(node.children.sift[Node.Data]))

enum Multiplicity:
  case One, AtLeastOne, Optional, Many

  def combine(that: Multiplicity): Multiplicity = this match
    case One => One
    case Many => that
    case AtLeastOne => that match
      case One | Optional    => One
      case AtLeastOne | Many => AtLeastOne
    case Optional => that match
      case One | AtLeastOne  => One
      case Optional | Many   => Optional