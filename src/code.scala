package codala

import java.io.*
import scala.io.*
import annotation.tailrec
import language.dynamics

// LIghtweight COllaborative DAta

val source: String = """
schema irk 1
default build

Command build
  build one/core
  watch

Command publish
  clean
  call build
  publish maven-central one/core one/uri

Stream current
  Repo rudiments git@github.com:propensive/rudiments abcd1234
  Repo gossamer git@github.com:propensive/gossamer 573938383
  inherit one/build.irk

  Project one
    inherit current
    name           Scala ONE
    description    A collection of typesafe libraries for Scala 3
    Module core
      name One-Core
      compiler scala 
      sources src/core
      version 0.4.0
      require rudiments/core gossamer/core
      docs doc
    Module uri
      name One-URI
      sources src/uri
      version 0.3.0
      compiler scala
""" 

val keys: Array[String] = Array("schema", "default", "Command", "clean", "watch", "build", "call", "publish", "Stream", "Repo", "inherit", "Project", "Module", "name", "description", "id", "version", "require", "Artifact", "docs", "path", "type", "main", "compiler", "username", "group", "url", "organization", "git", "Developer", "url", "sources", "Target")
val keyMap = keys.zipWithIndex.to(Map)

object Binary:
  def write(number: Int, out: OutputStream): Unit =
    if number >= 254 then
      out.write(31.toByte)
      out.write((number >> 24).toByte)
      out.write((number >> 16).toByte)
      out.write((number >> 8).toByte)
      out.write(number.toByte)
    else out.write((number + 32).toByte)

  def write(nodes: List[Node], out: OutputStream): Unit =
    out.write(177.toByte) // b1
    out.write(192.toByte) // c0
    out.write(218.toByte) // da
    out.write(28.toByte)  // 1a
    write(nodes.length, out)
    nodes.foreach(_.write(out))
    out.close()

  def read(in: InputStream): List[Node] =
    if in.read != 177 then throw Exception("Expected 0xb1")
    if in.read != 192 then throw Exception("Expected 0xc0")
    if in.read != 218 then throw Exception("Expected 0xda")
    if in.read != 28 then throw Exception("Expected 0x1a")
    (0 until readNumber(in)).map(_ => Node.read(in)).to(List)

  def readNumber(in: InputStream): Int =
    in.read match
      case 31 => (in.read << 24) + (in.read << 16) + (in.read << 8) + in.read
      case num => num - 32

object Token:
  def apply(value: String): Token = Token(0, value, 0)
  
  def read(in: InputStream): Token =
    val buf = new Array[Byte](Binary.readNumber(in))
    in.read(buf)
    Token(0, String(buf, "UTF-8"), Binary.readNumber(in))

case class Token(start: Int, value: String, padding: Int):
  def end: Int = start + value.length
  override def toString(): String = (" "*padding)+value

  def write(out: OutputStream): Unit =
    Binary.write(value.length, out)
    out.write(value.getBytes("UTF-8"))
    Binary.write(padding, out)

object Node:
  def read(in: InputStream): Node =
    val key = Token(keys(Binary.readNumber(in)))
    val values = (0 until Binary.readNumber(in)).map(_ => Token.read(in))
    val children = (0 until Binary.readNumber(in)).map(_ => Node.read(in))
    Node(key, values.to(List), children.to(List))

case class Node(key: Token, params: List[Token], children: List[Node]):
  def write(out: OutputStream): Unit =
    Binary.write(keyMap(key.value), out)
    Binary.write(params.length, out)
    params.foreach(_.write(out))
    Binary.write(children.length, out)
    children.foreach(_.write(out))

  def render(indent: Int = 0): Unit =
    print(" "*(indent*2))
    print(key.value)
    print(" ")
    println(params.map(_.toString).mkString(" "))
    children.foreach(_.render(indent + 1))

case class Line(indent: Int, values: List[Token], comment: Option[Token]):
  override def toString(): String = s"${"  "*indent}${values.mkString(" ")}${comment.fold("")("#"+_)}"
  def node: Node = Node(values.head, values.tail, Nil)

case class Comment(pos: Int, indent: Int, value: String)

@tailrec
def merge(lines: List[Line], data: List[List[Node]]): List[Node] =
  lines match
    case Nil => data match
      case xs :: (h :: t) :: tail =>
        merge(Nil, (h.copy(children = xs.reverse) :: t) :: tail)
      case xs :: Nil =>
        xs.reverse
    case next :: rest => (data.length - next.indent) match
      case 1 =>
        merge(rest, (next.node :: data.head) :: data.tail)
      case 0 =>
        merge(rest, List(next.node) :: data)
      case n if n > 1 => data match
        case xs :: (h :: t) :: tail =>
          merge(lines, (h.copy(children = xs.reverse) :: t) :: tail)

def parse(reader: Reader): List[Node] =
  val buf = StringBuilder()
  @tailrec
  def recur(pos: Int, tokens: List[Token], lines: List[Line], lineStart: Int, comment: Boolean, wipe: Boolean, padding: Int): List[Line] =

    if wipe then buf.clear()
    
    lazy val newTokens: List[Token] = if buf.isEmpty || comment then tokens else Token(pos - buf.length, buf.toString, padding) :: tokens
     
    lazy val indent =
      val count = newTokens.lastOption.fold(pos)(_.start) - lineStart
      if count%2 == 1 then throw Exception("Even number of spaces required at position "+pos) else count/2

    lazy val newLines = if comment then Line(indent, newTokens, Some(Token(pos - buf.length, buf.toString, 0))) :: lines else if newTokens.isEmpty then lines else Line(indent, newTokens.reverse, None) :: lines
    
    reader.read() match
      case -1 =>
        lines.reverse
      case '\n' =>
        recur(pos + 1, Nil, newLines, pos + 1, false, true, 0)
      case char if comment =>
        buf.append(char.toChar)
        recur(pos + 1, tokens, lines, lineStart, true, false, 0)
      case ' ' =>
        recur(pos + 1, newTokens, lines, lineStart, false, true, if !buf.isEmpty then 0 else padding + 1)
      case '#' =>
        recur(pos + 1, Nil, newLines, pos + 1, true, true, 0)
      case char =>
        buf.append(char.toChar)
        recur(pos + 1, tokens, lines, lineStart, false, false, padding)

  merge(recur(0, Nil, Nil, 0, false, false, 0), List(Nil))

@main
def run(): Unit =
  val reader: Reader = StringReader(source)
  val result = parse(reader)
  val schemaNodes = parse(StringReader(IrkSchemaSource))
  val schema: Schema = Schema("irk", true, false, Nil, Schema.parse(schemaNodes))
  println(schema)

  val file = File("/home/jpretty/tokeniser/out.bcdl")
  val file2 = File("/home/jpretty/tokeniser/out.cdl")
  val fos = new FileOutputStream(file)
  val writer = new FileWriter(file2)
  writer.write(source)
  writer.close()

  result.foreach(_.render())

  Binary.write(result, fos)
  val fis = BufferedInputStream(new FileInputStream(file))
  Binary.read(fis).foreach(_.render())

  val structs = schema.validate(result)
  structs.foreach(_.render(0))

  val root = Struct(Token("root"), structs)

  println(root.Stream.Project.as[Project])
      
trait Serializer[T]:
  def apply(value: T): Struct

trait Deserializer[T]:
  def apply(struct: Struct): Option[T]


case class Module(id: String, name: String, sources: List[String])
case class Project(id: String, name: String, description: String, modules: List[Module])

given Deserializer[Module] = struct =>
  Some(Module(
    struct.id(),
    struct.name(),
    struct.sources.children.map(_())
  ))

given Deserializer[Project] = struct =>
  Some(Project(struct.id(), struct.name(), struct.description(), struct.children.filter(_.key.value == "Module").map(_.as[Module])))

object Schema:
  def parse(nodes: List[Node]): List[Schema] =
    nodes.map { node =>
      val repeated = node.key.value.endsWith("*") || node.key.value.endsWith("+")
      val optional = node.key.value.endsWith("?") || node.key.value.endsWith("*")
      val key = if repeated || optional then node.key.value.dropRight(1) else node.key.value
      node.params.reverse match
        case Nil => Schema(key, !optional, repeated, Nil, parse(node.children))
        case h :: t =>
          val pRepeat = h.value.endsWith("*") || h.value.endsWith("+")
          val pOptional = h.value.endsWith("?") || h.value.endsWith("*")
          val params = (h.value.dropRight(if pRepeat || pOptional then 1 else 0) :: t.map(_.value)).reverse
          Schema(key, !optional, repeated, params, parse(node.children), lastRepeatable = pRepeat, lastRequired = !pOptional)
    }


case class MissingParamsError(key: String, minimum: Int, pos: Int) extends Exception(s"the key $key requires at least $minimum parameter(s) at position $pos")
case class MissingKeyError(key: String, parent: String, pos: Int) extends Exception(s"the key $key is required inside $parent at position $pos")
case class InvalidKeyError(key: String, parent: String, pos: Int) extends Exception(s"the key $key is not valid inside $parent at position $pos")
case class DuplicateKeyError(key: String, pos: Int, firstPos: Int) extends Exception(s"the unique key $key first used at $firstPos is duplicated at $pos")
case class SurplusParamsError(key: String, maximum: Int, pos: Int) extends Exception(s"the key $key may have at most $maximum parameters(s) at position $pos")
case class Struct(key: Token, children: List[Struct]) extends Dynamic:
  def apply(): String = children.head.key.value
  def as[T](using deserializer: Deserializer[T]): T = deserializer(this).get

  def selectDynamic(key: String): Struct = children.find(_.key.value == key).get
  def applyDynamic(key: String)(): String = selectDynamic(key).apply()
  
  def render(indent: Int = 0): Unit =
    print("  "*indent)
    println(key.value)
    children.foreach(_.render(indent + 1))


case class Schema(key: String, required: Boolean, repeatable: Boolean, positional: List[String] = Nil, children: List[Schema] = Nil, lastRequired: Boolean = true, lastRepeatable: Boolean = false):
  def apply(token: Token): Schema =
    children.find(_.key == token.value).getOrElse(throw InvalidKeyError(token.value, key, token.start))
  
  def validate(nodes: List[Node]): List[Struct] =
    nodes.foldLeft(Map[String, Token]()) { case (map, node) =>
      if !apply(node.key).repeatable && map.contains(node.key.value) then throw DuplicateKeyError(node.key.value, node.key.start, map(node.key.value).start)
      else map.updated(node.key.value, node.key)
    }

    val counts = nodes.map(_.key.value).to(Set)
    children.filter(_.required).foreach { schema =>
      if !counts.contains(schema.key) then throw MissingKeyError(schema.key, key, nodes.last.key.end)
    }
      
    for node <- nodes yield
      val schema = apply(node.key)
      val minimum = if schema.lastRequired then schema.positional.length else schema.positional.length - 1
      if node.params.length < minimum then throw MissingParamsError(node.key.value, minimum, node.key.end)
      if !schema.lastRepeatable && node.params.length > schema.positional.length then throw SurplusParamsError(node.key.value, schema.positional.length, node.key.start)
     
      def recur(schemaParams: List[String], params: List[Token], list: List[Struct] = Nil): List[Struct] =
        schemaParams match
          case Nil          => list.reverse
          case head :: Nil  => Struct(Token(head), params.map(Struct(_, Nil))) :: list
          case head :: tail => recur(tail, params.tail, Struct(Token(head), List(Struct(params.head, Nil))) :: list)

      val paramStructs = recur(schema.positional, node.params)
      Struct(node.key, paramStructs ::: schema.validate(node.children))



lazy val IrkSchemaSource = """
schema id version
default cmd
Command* id
  build* id
  watch*
  clean*
  call* ref
  publish* target module+
Stream id
  Repo* id url hash
  inherit ref
  Project id
    inherit? ref
    name name+
    description desc+
    Module* id
      name name
      require? dependency+
      compiler? name
      sources paths+
      version number
      docs? paths*
"""
