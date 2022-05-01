package codala

import java.io.*
import scala.io.*
import annotation.tailrec
import language.dynamics
import gossamer.*
import rudiments.*
import eucalyptus.*

given Realm(t"codala")

object Bin:
  def write(out: Writer, number: Int): Unit = out.write((number + 32).toChar)

  def write(out: Writer, text: Text): Unit =
    write(out, text.length)
    out.write(text.s)

  def write(out: Writer, doc: ValidDoc): Unit throws CodalaValidationError =
    out.write("\u00b1\u00c0\u00da")
    write(out, doc.schema, doc.data)

  private def write(out: Writer, schema: Schema, structs: List[Struct])
                   : Unit throws CodalaValidationError =
    write(out, structs.length)
    
    structs.foreach:
      case Point(key, children) =>
        val idx: Int = schema.keyMap(key)
        write(out, idx)
        write(out, 0)
        write(out, schema.allSubschemas(idx), children)
      
      case KeyValue(key, value) =>
        val idx: Int = schema.keyMap(key)
        write(out, idx)
        write(out, value)
  
  def readDoc(schema: Schema, reader: Reader): ValidDoc throws BinaryError =
    if reader.read() != '\u00b1' || reader.read() != '\u00c0' || reader.read() != '\u00da'
    then throw BinaryError(t"header 0xb1c0da", 0)
    
    def recur(schema: Schema): List[Struct] =
      List.range(0, readNumber(reader)).map:
        idx =>
          val idx = readNumber(reader)
          val subschema = schema.allSubschemas(idx)
          Bin.readNumber(reader) match
            case 0 => Point(subschema.key, recur(subschema))
            case n => KeyValue(subschema.key, readText(reader, n))
    
    ValidDoc(schema, recur(schema))

  def readNumber(in: Reader): Int = in.read - 32

  def readText(in: Reader, length: Int = -1): Text =
    val buf = new Array[Char](if length == -1 then readNumber(in) else length)
    in.read(buf)
    String(buf).show
  
object Token:
  def apply(value: Text): Token = Token(0, value, 0)
  
case class Token(start: Int, value: Text, padding: Int = 0):
  def end: Int = start + value.length
  override def toString(): String = (value+(t" "*padding)).s

enum Node:
  case Doc(childNodes: List[Data | Blank])
  case Blank(start: Int, length: Int, leadingComment: Option[Text])
  case Data(key: Token, args: List[Token], childNodes: List[Data | Blank],
                trailingComment: Option[Text] = None, leadingComment: Option[Text])

  def data: List[Data] = children.sift[Data]

  def pos: Int = this match
    case Doc(nodes)            => nodes.headOption.map(_.pos).getOrElse(0)
    case Blank(pos, _, _)      => pos
    case Data(key, _, _, _, _) => key.start

  def children: List[Data | Blank] = this match
    case Doc(nodes)              => nodes
    case Blank(_, _, _)          => Nil
    case Data(_, _, nodes, _, _) => nodes

  def params: List[Token] = this match
    case Doc(_)                   => Nil
    case Blank(_, _, _)           => Nil
    case Data(_, params, _, _, _) => params

  def comment: Option[Text] = this match
    case Doc(_)                    => None
    case Blank(_, _, comment)      => comment
    case Data(_, _, _, _, comment) => comment

  def apply(): Text = this match
    case Data(key, params, _, _, _) => (key :: params).map(_.value).join(t" ")
    case Doc(children)              => t""
    case Blank(_, _, comment)       => comment.getOrElse(t"")

  def render(indent: Int = 0): Unit = this match
    case Blank(_, rows, comment) =>
      println()
    case Doc(children) =>
      children.foreach(_.render(0))
    case Data(key, params, children, comment, leadingComment) =>
      print(" "*(indent*2))
      print(key.value)
      print(" ")
      println(params.map(_.toString.show).join(t" "))
      children.foreach(_.render(indent + 1))

case class Comment(pos: Int, indent: Int, value: Text)

object Codala:
  def parse(text: Text): Node.Doc throws CodalaParseError = parse(StringReader(text.s))

  def parse(reader: Reader): Node.Doc throws CodalaParseError =
    import CodalaParseError.Indentation.*
    var pos: Int = 0
    var char: Int = reader.read()
    var initialPrefix: Int = 0
    val buffer: StringBuilder = StringBuilder()
    var comment: Boolean = false
    var commentOpt: Option[Text] = None
    var multiline: Int = -1
    var stack: List[Node.Data] = Nil
    var node: Node.Data = Node.Data(Token(t"ROOT"), Nil, Nil, None, None)
    var blank: Int = 0
    var blankStart: Int = 0
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
    
    def token(): Token =
      val start = pos
      while
        buffer.append(char.toChar)
        continue && next() && char != ' ' && char != '\n'
      do ()
      Token(start, get(), skip())
    
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
    
    def get(): Text =
      val text = buffer.toString.show
      buffer.clear()
      text

    def param(token: Token): Unit =
      if token.value == t"#" then
        comment = true
        line()
        node = node.copy(trailingComment = Some(get()))
      else node = node.copy(args = token :: node.args)

    while
      initialPrefix = skip()
      char == '\n'
    do next()

    def addBlank(): Unit = if blank > 0 then
      node = node.copy(childNodes = Node.Blank(blankStart, blank, commentOpt) :: node.childNodes)
      blank = 0
      commentOpt = None

    while char != -1 do
      val count = skip()
      val indent = count/2

      if char == '\n' then
        if comment then
          commentOpt = Some(get())
          comment = false
        if blank == 0 then blankStart = pos - count
        blank += 1
      else if currentIndent + 1 == indent then
        if comment then throw CodalaParseError(pos, AfterComment)
        if multiline > 0 then
          buffer.append('\n')
          for i <- (currentIndent + 1)*2 until count do buffer.append(' ')
        else if indent > currentIndent + 2 then throw CodalaParseError(pos, Surplus)
        multiline = pos
        line()
      else if char == '#' then
        next()
        skip()
        addBlank()
        if comment then buffer.append('\n') else comment = true
        line()
      else if count%2 == 1 && multiline == -1 then throw CodalaParseError(pos, Uneven)
      else
        if comment then
          commentOpt = Some(get())
          comment = false
       
        addBlank()
        
        if multiline > 0 then
          if !buffer.isEmpty then param(Token(multiline, get()))
          multiline = -1
        
        while indent < currentIndent do pop()
        val tok = token()
        stack ::= node
        
        if multiline > 0 && !buffer.isEmpty then
          param(token())
          multiline = -1
        
        node = Node.Data(tok, Nil, Nil, None, commentOpt)
        commentOpt = None
        currentIndent += 1
        while continue do param(token())
      
      next()
      
      def cue(limit: Int): Boolean =
        while continue && pos < limit && next() && char == ' ' do ()
        pos == limit
    
      if continue && !cue(pos + initialPrefix) && char != '\n' && continue
      then throw CodalaParseError(pos, Insufficient)

    if multiline > 0 && !buffer.isEmpty then param(Token(multiline, get()))
    if blank > 0 then node = node.copy(childNodes = Node.Blank(blankStart, blank, commentOpt) :: node.childNodes)

    while stack.length > 0 do pop()
    
    Node.Doc(node.children.reverse)

trait Serializer[T]:
  def apply(value: T): Struct

trait Deserializer[T]:
  def apply(struct: Struct): Option[T]

object Schema:
  def parse(doc: Node.Doc): Schema throws MultipleIdentifiersError = 
    Schema(t"", None, false, false, IArray(), IArray(parseNodes(doc.data)*), true, false)

  def parseNodes(data: List[Node.Data]): List[Schema] throws MultipleIdentifiersError =
    data.map:
      node =>
        val repeated = node.key.value.endsWith(t"*") || node.key.value.endsWith(t"+")
        val optional = node.key.value.endsWith(t"?") || node.key.value.endsWith(t"*")
        val key = if repeated || optional then node.key.value.drop(1, Rtl) else node.key.value
        
        val id = node.params.zipWithIndex.filter(_(0).value.endsWith(t"!")) match
          case List((id, n)) => Some(n)
          case Nil           => None
          case _             => throw MultipleIdentifiersError(key, node.key.start)
        
        // FIXME: Checks whether last param is id, not first
        node.params.reverse match
          case Nil =>
            Schema(key, None, !optional, repeated, IArray(), IArray(parseNodes(node.data)*))
          
          case param :: rest =>
            val variadic = param.value.endsWith(t"*") || param.value.endsWith(t"+")
            val skippable = param.value.endsWith(t"?") || param.value.endsWith(t"*")
            val drop = if variadic || skippable || param.value.endsWith(t"!") then 1 else 0
            val params = param.value.drop(drop, Rtl) :: rest.map(_.value)
            
            Schema(key, id, !optional, repeated, IArray(params.reverse*),
                IArray(parseNodes(node.data)*), variadic = variadic, allRequired = !skippable)

case class CodalaParseError(pos: Int, indentation: CodalaParseError.Indentation)
extends Error((t"could not parse Codala document at ", pos, t": ", indentation)):
  def message: Text = t"could not parse Codala document at $pos: $indentation"

object CodalaParseError:
  enum Indentation:
    case Uneven, AfterComment, Surplus, Insufficient

case class BinaryError(expectation: Text, pos: Int)
extends Exception(s"expected $expectation at position $pos")

object CodalaValidationError:
  enum Issue:
    case MissingParams(count: Int)
    case MissingKey(key: Text)
    case DuplicateKey(key: Text, firstPos: Int)
    case SurplusParams(count: Int)
    case InvalidKey(key: Text)
    case DuplicateId(id: Text, firstPos: Int)

import CodalaValidationError.Issue.*

case class CodalaValidationError(schema: Schema, pos: Int, issue: CodalaValidationError.Issue)
extends Error((t"the Codala document did not conform to the schema ", schema, t" at position ", pos,
    t" because ", issue)):
  def message: Text = t"the Codala document did not conform to the schema: ${issue.show}"

case class MultipleIdentifiersError(key: Text, pos: Int)
extends Exception(s"multiple parameters of $key have been marked as identifiers at position $pos")

sealed trait Struct:
  def key: Text
  def apply(idx: Int = 0): Text

case class KeyValue(key: Text, value: Text) extends Struct:
  def apply(idx: Int = 0): Text = value

case class Point(key: Text, children: List[Struct]) extends Struct, Dynamic:
  def as[T](using deserializer: Deserializer[T]): T = deserializer(this).get
  def selectDynamic(key: String): Struct = children.find(_.key == key.show).get
  def applyDynamic(key: String)(): Text = selectDynamic(key).apply(0)
  def apply(idx: Int = 0): Text = children(idx).key

object ValidDoc:
  def read(schema: Schema, text: Text): ValidDoc throws BinaryError =
    Bin.readDoc(schema, StringReader(text.s))

case class ValidDoc(schema: Schema, data: List[Struct]):
  def bin: Text =
    val writer = StringWriter()
    unsafely(Bin.write(writer, this))
    writer.toString.show


case class Schema(key: Text, identifier: Option[Int], required: Boolean, repeatable: Boolean,
                      params: IArray[Text] = IArray(), subschemas: IArray[Schema] = IArray(),
                      allRequired: Boolean = true, variadic: Boolean = false):
  
  lazy val allSubschemas = params.map(Schema(_, None, false, false)) ++ subschemas

  lazy val keyMap: Map[Text, Int] = allSubschemas.zipWithIndex.map:
    (sub, index) => sub.key -> index
  .to(Map)

  def apply(token: Token): Schema throws CodalaValidationError =
    keyMap.get(token.value).map(allSubschemas(_)).getOrElse:
      throw CodalaValidationError(this, token.start, InvalidKey(token.value))

  def validate(doc: Node.Doc): ValidDoc throws CodalaValidationError =
    ValidDoc(this, validation(doc))

  def validation(doc: Node.Doc): List[Struct] throws CodalaValidationError =
    doc.data.foldLeft(Map[Text, Token]()):
      case (map, node) =>
        if !apply(node.key).repeatable && map.contains(node.key.value)
        then
          val duplication = DuplicateKey(node.key.value, map(node.key.value).start)
          throw CodalaValidationError(this, node.key.start, duplication)
        else map.updated(node.key.value, node.key)

    val missing = doc.data.foldLeft(subschemas.filter(_.required).map(_.key).to(Set)):
      (missing, next) => missing - next.key.value

    if missing.nonEmpty
    then throw CodalaValidationError(this, doc.pos, MissingKey(missing.head))

    doc.data.foldLeft(Map[Text, Token]()):
      case (map, node) =>
        apply(node.key).identifier.map(node.params(_)).fold(map):
          ident =>
            if map.contains(ident.value)
            then
              val duplication = DuplicateId(ident.value, map(ident.value).start)
              throw CodalaValidationError(this, ident.start, duplication)
            else map.updated(ident.value, ident)

    for node <- doc.data yield
      val schema = apply(node.key)
      val minimum = if schema.allRequired then schema.params.length else schema.params.length - 1
      
      if node.params.length < minimum
      then throw CodalaValidationError(this, node.key.end, MissingParams(minimum))
      
      if !schema.variadic && node.params.length > schema.params.length
      then throw CodalaValidationError(this, node.key.start, SurplusParams(schema.params.length))

      def recur(schemaParams: List[Text], params: List[Text], list: List[Struct] = Nil)
               : List[Struct] =
        schemaParams match
          case Nil          => list.reverse
          //case head :: Nil  => Point(head, params.map(Struct(_, Nil))) :: list
          case head :: tail => recur(tail, params.tail, KeyValue(head, params.head) :: list)

      val paramStructs = recur(schema.params.to(List), node.params.map(_.value))
      Point(node.key.value, paramStructs ++ schema.validation(Node.Doc(node.children.sift[Node.Data])))

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