package codl

import java.io.*
import scala.io.*
import annotation.tailrec
import language.dynamics
import gossamer.*
import rudiments.*
import eucalyptus.*
import wisteria.*

given Realm(t"codl")

object Bin:
  def write(out: Writer, number: Int): Unit = out.write((number + 32).toChar)

  def write(out: Writer, text: Text): Unit =
    write(out, text.length)
    out.write(text.s)

  def write(out: Writer, doc: CodlDoc): Unit throws CodlValidationError =
    out.write("\u00b1\u00c0\u00da")
    write(out, doc.schema, doc.children)

  private def write(out: Writer, schema: Schema, structs: List[Struct])
                   : Unit throws CodlValidationError =
    write(out, structs.length)
    
    structs.foreach:
      case Space(length, comment) =>
        ()

      case Branch(key, _, children, leading, trailing) =>
        val idx: Int = schema.keyMap(key)
        write(out, idx)
        write(out, 0)
        write(out, schema.allSubschemas(idx), children)
      
      case Param(key, value, _, _, _, _) =>
        val idx: Int = schema.keyMap(key)
        write(out, idx)
        write(out, value)
  
  def readDoc(schema: Schema, reader: Reader): CodlDoc throws BinaryError =
    if reader.read() != '\u00b1' || reader.read() != '\u00c0' || reader.read() != '\u00da'
    then throw BinaryError(t"header 0xb1c0da", 0)
    
    def recur(schema: Schema): List[Struct] =
      List.range(0, readNumber(reader)).map:
        idx =>
          val idx = readNumber(reader)
          val subschema = schema.allSubschemas(idx)
          
          Bin.readNumber(reader) match
            case 0 => Branch(subschema.key, 0, recur(subschema), None, None)
            case n => val text = readText(reader, n)
                      val multiline = text.contains(' ') || text.contains('\n')
                      Param(subschema.key, text, multiline, 0, None, None)
    
    CodlDoc(schema, recur(schema), 0)

  def readNumber(in: Reader): Int = in.read - 32

  def readText(in: Reader, length: Int = -1): Text =
    val buf = new Array[Char](if length == -1 then readNumber(in) else length)
    in.read(buf)
    String(buf).show
  
object Token:
  def apply(value: Text): Token = Token(0, value, 0)
  
case class Token(start: Int, value: Text, padding: Int = 0, multiline: Boolean = false):
  def end: Int = start + value.length
  override def toString(): String = (value+(t" "*padding)).s

enum Node:
  case Root(initialPrefix: Int, childNodes: List[Data | Blank])
  case Blank(start: Int, length: Int, leading: Option[Text])
  case Data(key: Token, args: List[Token], childNodes: List[Data | Blank],
                trailing: Option[Text] = None, leading: Option[Text])

  def data: List[Data] = children.sift[Data]

  def pos: Int = this match
    case Root(_, nodes)        => nodes.headOption.map(_.pos).getOrElse(0)
    case Blank(pos, _, _)      => pos
    case Data(key, _, _, _, _) => key.start

  def children: List[Data | Blank] = this match
    case Root(_, nodes)          => nodes
    case Blank(_, _, _)          => Nil
    case Data(_, _, nodes, _, _) => nodes

  def params: List[Token] = this match
    case Root(_, _)               => Nil
    case Blank(_, _, _)           => Nil
    case Data(_, params, _, _, _) => params

  def comment: Option[Text] = this match
    case Root(_, _)                => None
    case Blank(_, _, comment)      => comment
    case Data(_, _, _, _, comment) => comment

  def apply(): Text = this match
    case Data(key, params, _, _, _) => (key :: params).map(_.value).join(t" ")
    case Root(_, children)          => t""
    case Blank(_, _, comment)       => comment.getOrElse(t"")

  def render(indent: Int = 0): Unit = this match
    case Blank(_, rows, comment) =>
      println()
    
    case Root(_, children) =>
      children.foreach(_.render(0))
    
    case Data(key, params, children, comment, leading) =>
      print(" "*(indent*2))
      print(key.value)
      print(" ")
      println(params.map(_.toString.show).join(t" "))
      children.foreach(_.render(indent + 1))

case class Comment(pos: Int, indent: Int, value: Text)

object Codl:
  def parse(text: Text): Node.Root throws CodlParseError = parse(StringReader(text.s))

  def parse(reader: Reader): Node.Root throws CodlParseError =
    import CodlParseError.Indentation.*
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
        node = node.copy(trailing = Some(get()))
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
        if comment then throw CodlParseError(pos, AfterComment)
        if multiline > 0 then
          buffer.append('\n')
          for i <- (currentIndent + 1)*2 until count do buffer.append(' ')
        else if indent > currentIndent + 2 then throw CodlParseError(pos, Surplus)
        multiline = pos
        line()
      else if char == '#' then
        next()
        //skip()
        addBlank()
        if comment then buffer.append('\n') else comment = true
        line()
      else if count%2 == 1 && multiline == -1 then throw CodlParseError(pos, Uneven)
      else
        if comment then
          commentOpt = Some(get())
          comment = false
       
        addBlank()
        
        if multiline > 0 then
          if !buffer.isEmpty then param(Token(multiline, get(), multiline = true))
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
      then throw CodlParseError(pos, Insufficient)

    if multiline > 0 && !buffer.isEmpty then param(Token(multiline, get(), multiline = true))
    if blank > 0 then node = node.copy(childNodes = Node.Blank(blankStart, blank, commentOpt) :: node.childNodes)

    while stack.length > 0 do pop()
    
    Node.Root(initialPrefix, node.children.reverse)

object Serializer extends Derivation[Serializer]:
  def join[T](ctx: CaseClass[Serializer, T]): Serializer[T] = value =>
    val children = ctx.params.map:
      param => param.typeclass(param.deref(value)).label(param.label.show)
    
    Branch(ctx.typeInfo.short.show, 0, children.to(List), None, None)
  
  def split[T](ctx: SealedTrait[Serializer, T]): Serializer[T] = ???

  given [T: Show]: Serializer[T] = value =>
    val text = value.show
    val multiline = text.contains(' ') || text.contains('\n')
    Param(t"", text, multiline, 0, None, None)

trait Serializer[T]:
  def apply(value: T): Struct

object Deserializer extends Derivation[Deserializer]:
  def join[T](ctx: CaseClass[Deserializer, T]): Deserializer[T] = value => Some:
    value match
      case branch@Branch(key, _, children, _, _) =>
        ctx.construct:
          param => param.typeclass(branch.selectDynamic(param.label)).get
    
  def split[T](ctx: SealedTrait[Deserializer, T]): Deserializer[T] = ???

  given Deserializer[Text] =
    case Param(key, value, _, _, _, _) => Some(value)
    case _ => None

  given Deserializer[String] =
    case Param(key, value, _, _, _, _) => Some(value.s)
    case _ => None

  given Deserializer[Int] =
    case Param(key, value, _, _, _, _) => Int.unapply(value)
    case _ => None

trait Deserializer[T]:
  def apply(struct: Struct): Option[T]

object SchemaGen extends Derivation[SchemaGen]:
  def join[T](ctx: CaseClass[SchemaGen, T]): SchemaGen[T] = () =>
    Schema(ctx.typeInfo.short.show, None, true, false, IArray(),
        IArray(ctx.params.map { param => param.typeclass.schema().copy(key = param.label.show) }*), true, false, false)
  
  def split[T](ctx: SealedTrait[SchemaGen, T]): SchemaGen[T] = () => ???

  given SchemaGen[Text] = () =>
    Schema(t"Text", None, true, false, IArray(), IArray(), true, false, false)
  
  given SchemaGen[Int] = () =>
    Schema(t"Int", None, true, false, IArray(), IArray(), true, false, false)

// case class Schema(key: Text, identifier: Option[Int], required: Boolean, repeatable: Boolean,
//                       params: IArray[Text] = IArray(), subschemas: IArray[Schema] = IArray(),
//                       allRequired: Boolean = true, variadic: Boolean = false,
//                       unitary: Boolean = false):

trait SchemaGen[T]:
  def schema(): Schema

extension [T: SchemaGen: Serializer](value: T) def codl: CodlDoc = CodlDoc(value)

object Schema:
  def parse(doc: Node.Root): Schema throws MultipleIdentifiersError = 
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

case class CodlParseError(pos: Int, indentation: CodlParseError.Indentation)
extends Error((t"could not parse CoDL document at ", pos, t": ", indentation)):
  def message: Text = t"could not parse CoDL document at $pos: $indentation"

object CodlParseError:
  enum Indentation:
    case Uneven, AfterComment, Surplus, Insufficient

case class BinaryError(expectation: Text, pos: Int)
extends Exception(s"expected $expectation at position $pos")

object CodlValidationError:
  enum Issue:
    case MissingParams(count: Int)
    case MissingKey(key: Text)
    case DuplicateKey(key: Text, firstPos: Int)
    case SurplusParams(count: Int)
    case InvalidKey(key: Text)
    case DuplicateId(id: Text, firstPos: Int)

import CodlValidationError.Issue.*

case class CodlValidationError(schema: Schema, pos: Int, issue: CodlValidationError.Issue)
extends Error((t"the CoDL document did not conform to the schema ", schema, t" at position ", pos,
    t" because ", issue)):
  def message: Text = t"the CoDL document did not conform to the schema: ${issue.show}"

case class MultipleIdentifiersError(key: Text, pos: Int)
extends Exception(s"multiple parameters of $key have been marked as identifiers at position $pos")

case class MissingValueError(key: Text)
extends Error((t"the key ", key, t" does not exist in the CoDL document")):
  def message: Text = t"the key $key does not exist in the CoDL document"

sealed trait Struct:
  def key: Text
  def apply(idx: Int = 0): Struct
  def label(key: Text): Struct

case class Param(key: Text, value: Text, multiline: Boolean = false, padding: Int = 0,
                     leading: Option[Text] = None, trailing: Option[Text] = None)
extends Struct:
  def apply(idx: Int): Struct = ???
  def set(newValue: Text): Param = Param(key, newValue, multiline, padding, leading, trailing)
  def label(key: Text): Param = Param(key, value, multiline, padding, leading, trailing)

case class Space(length: Int, comment: Option[Text]) extends Struct:
  def children = Nil
  def apply(idx: Int): Struct = ???
  def key: Text = t""
  def label(key: Text): Space = this

case class Branch(key: Text, padding: Int = 0, children: List[Struct] = Nil,
                      leading: Option[Text] = None, trailing: Option[Text] = None)
extends Struct, Dynamic:
  
  def as[T](using deserializer: Deserializer[T]): T = deserializer(this).get
  def selectDynamic(key: String): Struct = children.find(_.key == key.show).get
  def applyDynamic(key: String)(idx: Int): Struct = selectDynamic(key).apply(idx)
  def apply(idx: Int = 0): Struct = children(idx)
  def label(key: Text): Branch = Branch(key, padding, children, leading, trailing)

object CodlDoc:
  def apply[T](value: T)(using gen: SchemaGen[T], serializer: Serializer[T]) =
    serializer(value) match
      case Branch(key, value, children, _, _)  => new CodlDoc(gen.schema(), children, 0)
      case param@Param(key, value, _, _, _, _) => new CodlDoc(gen.schema(), List(param), 0)

  def read(schema: Schema, text: Text): CodlDoc throws BinaryError =
    Bin.readDoc(schema, StringReader(text.s))

  given Show[CodlDoc] = doc =>
    val writer = StringWriter()
    doc.serialize(writer)
    writer.toString.show

case class CodlDoc(schema: Schema, children: List[Struct], initialPrefix: Int) extends Dynamic:
  def selectDynamic(key: String): Struct = children.find(_.key == key.show).get
  def applyDynamic(key: String)(idx: Int): Struct = selectDynamic(key).apply(idx)
  def apply(idx: Int): Struct = children(idx)

  def bin: Text =
    val writer = StringWriter()
    unsafely(Bin.write(writer, this))
    writer.toString.show

  def serialize(out: Writer): Unit =
    def cue(indent: Int) = for i <- 0 until indent do out.write(' ')

    def print(indent: Int, data: List[Struct], first: Boolean): Unit =
      var oneLine: Boolean = !first
      var initial: Boolean = first
      def newline(): Unit = out.write('\n')
    
      def commentLines(indent: Int, text: Option[Text]): Unit =
        text.foreach:
          text =>
            text.cut('\n').foreach:
              line =>
                cue(indent)
                out.write('#')
                out.write(line.s)
                newline()
      val last = data.length - 1
      data.zipWithIndex.foreach:
        case (Space(length, leading), idx) =>
          commentLines(indent, leading)
          for i <- 0 until length do newline()
        
        case (Branch(key, padding, children, leading, trailing), idx) =>
          if !initial then newline()
          commentLines(indent, leading)
          cue(indent)
          out.write(key.s)
          for i <- 0 until padding do out.write(' ')
          
          trailing.foreach:
            comment =>
              out.write(" # ")
              out.write(comment.s)
          
          print(indent + 2, children, false)
          oneLine = false
        
        case (Param(key, value, multiline, padding, leading, trailing), idx) =>
          if oneLine then
            if multiline then
              if idx != last then
                newline()
                cue(indent)
                out.write(key.s)
              
              oneLine = false
              value.cut('\n').foreach:
                line =>
                  newline()
                  cue(indent + (if idx == last then 2 else 4))
                  out.write(line.s)
            else
              out.write(' ')
              out.write(value.s)
            
            trailing.foreach:
              comment =>
                oneLine = false
                out.write("# ")
                out.write(comment.s)
                newline()
          else
            commentLines(indent, leading)
            if !(initial && first) then newline()
            initial = false
            cue(indent)
            out.write(key.s)
            
            if multiline then
              oneLine = false
              value.cut('\n').foreach:
                line =>
                  newline()
                  cue(indent + 4)
                  out.write(line.s)
            else
              out.write(' ')
              out.write(value.s)
          
    print(initialPrefix, children, true)
    out.write('\n')
          
case class Schema(key: Text, identifier: Option[Int], required: Boolean, repeatable: Boolean,
                      params: IArray[Text] = IArray(), subschemas: IArray[Schema] = IArray(),
                      allRequired: Boolean = true, variadic: Boolean = false,
                      unitary: Boolean = false):
  
  lazy val allSubschemas = params.map(Schema(_, None, false, false, unitary = true)) ++ subschemas

  lazy val keyMap: Map[Text, Int] = allSubschemas.zipWithIndex.map:
    (sub, index) => sub.key -> index
  .to(Map)

  def apply(token: Token): Schema throws CodlValidationError =
    keyMap.get(token.value).map(allSubschemas(_)).getOrElse:
      throw CodlValidationError(this, token.start, InvalidKey(token.value))

  def validate(doc: Node.Root): CodlDoc throws CodlValidationError =
    CodlDoc(this, validation(doc), doc.initialPrefix)

  private def validation(doc: Node.Root): List[Struct] throws CodlValidationError =
    doc.data.foldLeft(Map[Text, Token]()):
      case (map, node) =>
        if !apply(node.key).repeatable && map.contains(node.key.value)
        then
          val duplication = DuplicateKey(node.key.value, map(node.key.value).start)
          throw CodlValidationError(this, node.key.start, duplication)
        else map.updated(node.key.value, node.key)

    val missing = doc.data.foldLeft(subschemas.filter(_.required).map(_.key).to(Set)):
      (missing, next) => missing - next.key.value

    if missing.nonEmpty
    then throw CodlValidationError(this, doc.pos, MissingKey(missing.head))

    doc.data.foldLeft(Map[Text, Token]()):
      case (map, node) =>
        apply(node.key).identifier.map(node.params(_)).fold(map):
          ident =>
            if map.contains(ident.value)
            then
              val duplication = DuplicateId(ident.value, map(ident.value).start)
              throw CodlValidationError(this, ident.start, duplication)
            else map.updated(ident.value, ident)

    for node <- doc.children yield
      node match
        case Node.Blank(start, length, leading) =>
          Space(length, leading)
        case Node.Data(key, params, childNodes, trailing, leading) =>
          val schema = apply(key)
          val minimum = if schema.allRequired then schema.params.length else schema.params.length - 1
          
          if params.length < minimum
          then throw CodlValidationError(this, key.end, MissingParams(minimum))
          
          if !schema.variadic && params.length > schema.params.length
          then throw CodlValidationError(this, key.start, SurplusParams(schema.params.length))
    
          def recur(schemaParams: List[Text], params: List[Token], list: List[Struct] = Nil)
                   : List[Struct] =
            schemaParams match
              case Nil =>
                list.reverse
              
              case head :: tail =>
                val param = Param(head, params.head.value, params.head.multiline, params.head.padding, None, None)
                recur(tail, params.tail, param :: list)
    
          val paramStructs = recur(schema.params.to(List), params, Nil)
          val childStructs = schema.validation(Node.Root(doc.initialPrefix, childNodes))
          
          Branch(key.value, key.padding - 1, paramStructs ++ childStructs, leading, trailing)

enum Multiplicity:
  case One, AtLeastOne, Optional, Many

  def combine(that: Multiplicity): Multiplicity = this match
    case One        => One
    case Many       => that
    case AtLeastOne => that match
      case One | Optional    => One
      case AtLeastOne | Many => AtLeastOne
    case Optional   => that match
      case One | AtLeastOne  => One
      case Optional | Many   => Optional
