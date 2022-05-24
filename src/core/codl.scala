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

  // def write(out: Writer, doc: CodlDoc): Unit throws CodlValidationError =
  //   out.write("\u00b1\u00c0\u00d1")
  //   write(out, Subschema(t"", doc.schema.subschemas, Multiplicity.One), doc.children)

  // private def write(out: Writer, schema: Subschema, structs: List[Struct])
  //                  : Unit throws CodlValidationError =
  //   write(out, structs.length)
    
  //   structs.foreach:
  //     case Space(length, comment) =>
  //       ()

  //     case Branch(key, _, children, leading, trailing) =>
  //       val idx: Int = schema.keyMap(key)
  //       write(out, idx)
  //       write(out, 0)
  //       write(out, schema.subschemas(idx), children)
      
  //     case Param(key, values, _, _, _, _) =>
  //       val idx: Int = schema.keyMap(key)
  //       write(out, idx)
  //       write(out, values.length)
  //       values.foreach(write(out, _))
  
  // def readDoc(schema: RootSchema, reader: Reader): CodlDoc throws BinaryError =
  //   if reader.read() != '\u00b1' || reader.read() != '\u00c0' || reader.read() != '\u00d1'
  //   then throw BinaryError(t"header 0xb1c0d1", 0)
    
  //   def recur(schemas: List[Subschema]): List[Struct] =
  //     List.range(0, readNumber(reader)).map:
  //       idx =>
  //         val idx = readNumber(reader)
  //         val subschema = schemas(idx)
          
  //         Bin.readNumber(reader) match
  //           case 0 => Branch(subschema.key, 0, recur(subschema.subschemas), None, None)
  //           case n => val values = List.range(0, n).map { _ => readText(reader) }
  //                     val multiline = values.exists { text => text.contains(' ') || text.contains('\n') }
  //                     Param(subschema.key, values, multiline, 0, None, None)
    
  //   CodlDoc(RootSchema(schema.subschemas), recur(schema.subschemas), 0)

  def readNumber(in: Reader): Int = in.read - 32

  def readText(in: Reader, length: Int = -1): Text =
    val buf = new Array[Char](if length == -1 then readNumber(in) else length)
    in.read(buf)
    String(buf).show
  
// object Token:
//   def apply(value: Text): Token = Token(0, value, 0)
  
// case class Token(start: Int, value: Text, padding: Int = 0, multiline: Boolean = false):
//   def end: Int = start + value.length
//   override def toString(): String = (value+(t" "*padding)).s

// enum Node:
//   case Root(initialPrefix: Int, childNodes: List[Data | Blank])
//   case Blank(start: Int, length: Int, leading: Option[Text])
//   case Data(key: Token, args: List[Token], childNodes: List[Data | Blank],
//                 trailing: Option[Text] = None, leading: Option[Text])

//   def data: List[Data] = children.sift[Data]

//   // def as[T: Deserializer: SchemaGen]: T throws CodlValidationError = this match
//   //   case root@ Root(_, _) => summon[SchemaGen[T]].schema().validate(root).as[T]

//   def pos: Int = this match
//     case Root(_, nodes)        => nodes.headOption.map(_.pos).getOrElse(0)
//     case Blank(pos, _, _)      => pos
//     case Data(key, _, _, _, _) => key.start

//   def children: List[Data | Blank] = this match
//     case Root(_, nodes)          => nodes
//     case Blank(_, _, _)          => Nil
//     case Data(_, _, nodes, _, _) => nodes

//   def params: List[Token] = this match
//     case Root(_, _)               => Nil
//     case Blank(_, _, _)           => Nil
//     case Data(_, params, _, _, _) => params

//   def comment: Option[Text] = this match
//     case Root(_, _)                => None
//     case Blank(_, _, comment)      => comment
//     case Data(_, _, _, _, comment) => comment

//   def apply(): Text = this match
//     case Data(key, params, _, _, _) => (key :: params).map(_.value).join(t" ")
//     case Root(_, children)          => t""
//     case Blank(_, _, comment)       => comment.getOrElse(t"")

//   def render(indent: Int = 0): Unit = this match
//     case Blank(_, rows, comment) =>
//       println()
    
//     case Root(_, children) =>
//       children.foreach(_.render(0))
    
//     case Data(key, params, children, comment, leading) =>
//       print(" "*(indent*2))
//       print(key.value)
//       print(" ")
//       println(params.map(_.toString.show).join(t" "))
//       children.foreach(_.render(indent + 1))

// case class Comment(pos: Int, indent: Int, value: Text)

// object Serializer extends Derivation[Serializer]:
//   def join[T](ctx: CaseClass[Serializer, T]): Serializer[T] = value =>
//     val children = ctx.params.map:
//       param => param.typeclass(param.deref(value)).label(param.label.show)
    
//     Node(ctx.typeInfo.short.show, 0, children.to(List), None, None)
  
//   def split[T](ctx: SealedTrait[Serializer, T]): Serializer[T] = ???

//   given [T: Show]: Serializer[T] = value =>
//     val text = value.show
//     val multiline = text.contains(' ') || text.contains('\n')
//     Value(t"", List(text), multiline, 0, None, None)

// trait Serializer[T]:
//   def apply(value: T): Node
//   def multiplicity: Multiplicity = Multiplicity.One

// object Deserializer extends Derivation[Deserializer]:
//   def join[T](ctx: CaseClass[Deserializer, T]): Deserializer[T] = value => Some:
//     value match
//       case branch@Branch(key, _, children, _, _) =>
//         ctx.construct:
//           param => param.typeclass(branch.selectDynamic(param.label)).get
    
//   def split[T](ctx: SealedTrait[Deserializer, T]): Deserializer[T] = ???

//   given Deserializer[Text] =
//     case Param(key, values, _, _, _, _) => Some(values.head)
//     case _ => None

//   given Deserializer[List[Text]] =
//     case Node(key, values, _, _, _, _) => Some(values)
//     case _ => None
  
//   given Deserializer[Int] =
//     case Node(key, value-, _, _, _, _) => Int.unapply(values.head)
//     case _ => None

// trait Deserializer[T]:
//   def apply(node: Node): Option[T]

// object SchemaGen extends Derivation[SchemaGen]:
//   def join[T](ctx: CaseClass[SchemaGen, T]): SchemaGen[T] = () =>
//     val children = ctx.params.to(List).map:
//       param => Subschema(param.label.show, param.typeclass.schema().subschemas, Multiplicity.One)
    
//     RootSchema(children)
  
//   def split[T](ctx: SealedTrait[SchemaGen, T]): SchemaGen[T] = () => ???

//   given SchemaGen[Text] = () => RootSchema(List(Value))
//   given SchemaGen[Int] = () => RootSchema(List(Value))

// trait SchemaGen[T]:
//   def schema(): RootSchema

//extension [T: SchemaGen: Serializer](value: T) def codl: CodlDoc = CodlDoc(value)

// object Subschema:
//   def parse(doc: Node.Root): RootSchema throws MultipleIdentifiersError = 
//     def parseNodes(data: List[Node.Data]): List[Subschema] throws MultipleIdentifiersError =
//       data.map:
//         node =>
//           val multiplicity = Multiplicity.parse(node.key.value.last)
//           val key = if multiplicity == Multiplicity.One then node.key.value else node.key.value.drop(1, Rtl)
          
//           val paramSchemas = node.params.map:
//             param =>
//               val multiplicity = Multiplicity.parse(param.value.last)
//               val key = if multiplicity == Multiplicity.One then param.value else param.value.drop(1, Rtl)

//               Subschema(key, List(Value), multiplicity)
          
//           if paramSchemas.count(_.multiplicity == Multiplicity.Unique) > 1
//           then throw MultipleIdentifiersError(key)
          
//           Subschema(key, paramSchemas ++ parseNodes(node.data), multiplicity)
    
//     RootSchema(parseNodes(doc.data))

case class CodlParseError(pos: Int, indentation: CodlParseError.Indentation)
extends Error((t"could not parse CoDL document at ", pos, t": ", indentation)):
  def message: Text = t"could not parse CoDL document at $pos: $indentation"


object CodlParseError:
  enum Indentation:
    case Uneven(initial: Int, indent: Int)
    case AfterComment, Surplus, Insufficient

case class BinaryError(expectation: Text, pos: Int)
extends Exception(s"expected $expectation at position $pos")

object CodlValidationError:
  enum Issue:
    case MissingKey(key: Text | SpecialKey)
    case DuplicateKey(key: Text, firstPos: Int)
    case SurplusParams
    case RequiredAfterOptional
    case InvalidKey(key: Text | SpecialKey)
    case DuplicateId(id: Text)

import CodlValidationError.Issue.*

case class CodlValidationError(word: Text | SpecialKey, issue: CodlValidationError.Issue)
extends Error((t"the CoDL document did not conform to the schema at ", word, t" because ", issue)):
  def message: Text = t"the CoDL document did not conform to the schema: ${issue.show}"

case class MultipleIdentifiersError(key: Text)
extends Exception(s"multiple parameters of $key have been marked as identifiers")

case class MissingValueError(key: Text)
extends Error((t"the key ", key, t" does not exist in the CoDL document")):
  def message: Text = t"the key $key does not exist in the CoDL document"

sealed trait Schema:
  lazy val keyMap: Map[Text | SpecialKey, Int] = subschemas.zipWithIndex.map(_.key -> _).to(Map)
  def subschemas: List[Subschema]
  def freeform = keyMap.contains(SpecialKey.UntypedNode) || keyMap.contains(SpecialKey.UntypedValue)
  
  def apply(key: Text): Schema throws CodlValidationError =
    if freeform then RootSchema.Freeform else keyMap.get(key).map(subschemas(_)).getOrElse:
      throw CodlValidationError(key, InvalidKey(key))

object Value extends Subschema(t"", Nil, Multiplicity.One)

object RootSchema:
  val Freeform = RootSchema(List(Subschema(SpecialKey.UntypedNode, List(Subschema(
      SpecialKey.UntypedValue, Nil, Multiplicity.Many)), Multiplicity.Many)))

case class RootSchema(subschemas: List[Subschema]) extends Schema

case class Subschema(key: Text | SpecialKey, subschemas: List[Subschema], multiplicity: Multiplicity)
extends Schema:
  def identifier: Option[Subschema] = subschemas.find(_.multiplicity == Multiplicity.Unique)

enum SpecialKey:
  case UntypedValue, UntypedNode

object Multiplicity:
  def parse(symbol: Char): Multiplicity = symbol match
    case '+' => Multiplicity.AtLeastOne
    case '?' => Multiplicity.Optional
    case '*' => Multiplicity.Many
    case '&' => Multiplicity.Joined
    case '!' => Multiplicity.Unique
    case _   => Multiplicity.One

enum Multiplicity:
  case One, AtLeastOne, Optional, Many, Joined, Unique

  def required: Boolean = this match
    case One | Unique | AtLeastOne => true
    case Optional | Many | Joined  => false

  def variadic: Boolean = this match
    case Joined | AtLeastOne | Many => true
    case Optional | Unique | One    => false
  
  def repeatable: Boolean = this match
    case AtLeastOne | Many => true
    case _                 => false

opaque type Character = Int
object Character:
  val End: Character = Character(-1)
  def apply(int: Int): Character = int
  
  given Typeable[Character] with
    def unapply(value: Any): Option[value.type & Character] = value match
      case char: Char => Some(value.asInstanceOf[value.type & Character])
      case _          => None

  erased given CanEqual[Char, Character] = compiletime.erasedValue
  erased given CanEqual[Character, Char] = compiletime.erasedValue

  extension (char: Character)
    def char: Char = if char == -1 then '\u0000' else char.toChar
    def int: Int = char

object Tokenizer:
  enum Token:
    case Word(text: Text, position: Int)
    case Padding(count: Int, position: Int)
    case Newline(position: Int)

    def serialize: Text = this match
      case Word(text, _)     => text
      case Padding(count, _) => t" "*count
      case Newline(_)        => t"\n"

    def whitespace: Boolean = this match
      case Word(_, _) => false
      case _          => true
    
    def newline: Boolean = this match
      case Newline(_) => true
      case _          => false
  
class Tokenizer(private val in: Reader):
  private var pos: Int = 0
  private val buf: StringBuilder = StringBuilder()
  private var cur: Character = Character(in.read())
  private var spaces: Int = 0

  private inline def read(): Unit =
    cur = Character(in.read())
    pos += 1

  @tailrec
  private def readSpaces(count: Int = 0): Int = if cur != ' ' then count else
    read()
    readSpaces(count + 1)


  @tailrec
  private def readWord(): Text =
    if cur == ' ' || cur == Character.End || cur == '\n' then buf.toString.show else
      buf.append(cur.char)
      read()
      readWord()
    
  def stream(): LazyList[Tokenizer.Token] = cur match
    case Character.End => LazyList()
    case '\n'          => read()
                          Tokenizer.Token.Newline(pos - 1) #:: stream()
    case ' '           => val count = readSpaces()
                          Tokenizer.Token.Padding(count, pos - count) #:: stream()
    case other         => buf.clear()
                          val word = readWord()
                          Tokenizer.Token.Word(word, pos - word.length) #:: stream()

case class Doc(schema: RootSchema, children: List[Point])
trait Point:
  def children: List[Point]

trait Data extends Point:
  def key: Text | SpecialKey

case class Gap(comments: List[Text], lines: Int) extends Point:
  def children: List[Point] = Nil

case class Node(key: Text | SpecialKey, params: List[Value] = Nil, lineComment: Option[Text] = None,
                      content: Option[Value] = None, children: List[Point] = Nil,
                      comments: List[Text] = Nil)
extends Data

case class Value(key: Text | SpecialKey, padding: Int, value: Text) extends Data:
  def children: List[Point] = Nil

enum UntypedNode:
  case Data(key: Text, data: List[Tokenizer.Token] = Nil, longLines: List[Text] = Nil,
                children: List[UntypedNode] = Nil, comment: List[Text] = Nil)

  case Blank(count: Int, comment: List[Text] = Nil)

object Codl:
  def parse(text: Text): Doc throws CodlParseError | CodlValidationError =
    parse(StringReader(text.s), RootSchema.Freeform)

  def parse(reader: Reader, schema: RootSchema): Doc throws CodlValidationError | CodlParseError =
    val tokenizer = Tokenizer(reader)
    val stream = tokenizer.stream()
    
    val initPrefix = stream.takeWhile(_.whitespace).lastOption match
      case Some(Tokenizer.Token.Padding(count, _)) => count
      case _                                       => 0
    
    @tailrec
    def trees(stream: LazyList[Tokenizer.Token], last: Int, focus: List[UntypedNode],
                  stack: List[List[UntypedNode]], comments: List[Text] = Nil, blanks: Int = 0,
                  multiline: Boolean = false): List[UntypedNode] =
      stream match
        case LazyList() => if stack.isEmpty then focus.reverse else stack match
          case (UntypedNode.Data(key, data, longLines, _, comment) :: neck) :: tail =>
            val node = UntypedNode.Data(key, data, longLines, focus.reverse, comment)
            trees(stream, 0, node :: neck, tail, Nil, blanks)
        
        case Tokenizer.Token.Word(_, pos) #:: rest =>
          val padding = Tokenizer.Token.Padding(0, pos)
          trees(padding #:: stream, last, focus, stack, comments, blanks)
        
        case Tokenizer.Token.Newline(_) #:: rest =>
          trees(rest, last, focus, stack, comments, blanks + 1, multiline)
        
        case Tokenizer.Token.Padding(_, _) #:: Tokenizer.Token.Newline(_) #:: rest =>
          val newBlanks = if multiline then 0 else blanks + 1
          trees(rest, last, focus, stack, comments, newBlanks, multiline)
        
        case Tokenizer.Token.Padding(count, _) #:: Tokenizer.Token.Word(key, _) #:: rest =>
          if blanks > 1 then
            trees(stream, last, UntypedNode.Blank(blanks - 1, comments) :: focus, stack)
          else
            lazy val data = rest.takeWhile(!_.newline).to(List)
            lazy val line = key+data.map(_.serialize).join
            lazy val surplus = rest.drop(data.length)
            
            def appendLine(text: Text) = focus match
              case UntypedNode.Data(key, args, longLines, children, comment) :: tail =>
                UntypedNode.Data(key, args, text :: longLines, children, comment) :: tail
            
            if multiline && (count - initPrefix > last*2 + 4) then
              val prefix = t" "*(count - initPrefix - last*2 - 4)
              trees(surplus, last, appendLine(prefix+line), stack, comments, 0, true)
            else
              val level = (count - initPrefix)/2
              if (count - initPrefix)%2 == 1 then ???
              
              (level - last) match
                case 2 => trees(surplus, last, appendLine(line), stack, comments, 0, true)
                case 1 => trees(stream, level, Nil, focus :: stack, Nil, blanks)
                
                case 0 => key.head match
                  case '#' => trees(surplus, last, focus, stack, line.drop(1) :: comments)
                  
                  case _ =>
                    val node = UntypedNode.Data(key, data, Nil, Nil, comments.reverse)
                    trees(surplus, level, node :: focus, stack)
                
                case n if n > 2 =>
                  ???
                
                case n if n < 0 => stack match
                  case (UntypedNode.Data(key, data, longLines, _, comment) :: neck) :: tail =>
                    val node = UntypedNode.Data(key, data, longLines, focus.reverse, comment)
                    trees(stream, last - 1, node :: neck, tail, Nil, blanks, multiline)
        
    def validate(schema: Schema, data: List[UntypedNode]): List[Point] = data.map:
      case UntypedNode.Data(key, args, longLines, children, comment) =>
        val subschema = schema(key)
        val cParams = args.dropWhile(_ != t"#").drop(1)
        val lineComment = if cParams.isEmpty then None else Some(cParams.map(_.serialize).join)
        val (last, parameters) = params(args, schema.subschemas, Nil, false)
        
        val content = if longLines.isEmpty then None else Some:
          last match
            case None       => ???
            case Some(last) => Value(last.key, 0, longLines.reverse.join(t"\n"))

        Node(key, parameters, lineComment, content, validate(subschema, children), comment)

      case UntypedNode.Blank(count, comments) => Gap(comments, count)

    @tailrec
    def params(todo: List[Tokenizer.Token], schemas: List[Subschema], values: List[Value], opt: Boolean): (Option[Subschema], List[Value]) =
      todo match
        case Nil | Tokenizer.Token.Padding(_, _) :: Nil =>
          schemas.headOption -> values
        
        case Tokenizer.Token.Padding(gap, _) :: Tokenizer.Token.Word(word, _) :: stillTodo => schemas match
          case Nil =>
            ???
          
          case subschema :: schemasTodo => subschema.multiplicity match
            case Multiplicity.One | Multiplicity.Unique =>
              if opt then
                ???
              
              params(stillTodo, schemasTodo, Value(subschema.key, gap, word) :: values, opt = false)
            
            case Multiplicity.Joined =>
              val content = word+stillTodo.map(_.serialize).join
              params(Nil, Nil, Value(subschema.key, gap, content) :: values, opt = opt)
            
            case Multiplicity.Optional =>
              params(stillTodo, schemasTodo, Value(subschema.key, gap, word) :: values, opt = true)
            
            case Multiplicity.Many | Multiplicity.AtLeastOne =>
              params(stillTodo, schemas, Value(subschema.key, gap, word) :: values, opt = opt)

    Doc(schema, validate(schema, trees(stream.dropWhile(_.whitespace), 0, Nil, Nil)))
