package codl

import java.io.*
import scala.io.*
import annotation.tailrec
import language.dynamics
import gossamer.*
import eucalyptus.*
import rudiments.*
import wisteria.*

given Realm(t"codl")

opaque type Character = Long
object Character:
  val End: Character = Long.MaxValue
  def apply(int: Int, line: Int, col: Int): Character =
    int.toLong | ((line.toLong&0xffffff) << 48) | ((col.toLong&0xffffff) << 24)
  
  given Typeable[Character] with
    def unapply(value: Any): Option[value.type & Character] = value match
      case char: Char => Some(value.asInstanceOf[value.type & Character])
      case _          => None

  erased given CanEqual[Char, Character] = compiletime.erasedValue
  erased given CanEqual[Character, Char] = compiletime.erasedValue

  extension (char: Character)
    def char: Char = if char == -1 then '\u0000' else char.toChar
    def line: Int = ((char >> 48) & 0xffffff).toInt
    def column: Int = ((char >> 24) & 0xffffff).toInt

import Character.*
import CodlParseError.Issue.*

class PositionReader(private val in: Reader):
  private var lastLine: Int = 0
  private var lastCol: Int = 0
  private var startLine: Int = 0
  private var startCol: Int = 0
  private var requireCr: Option[Boolean] = None
  private var finished: Boolean = false
  private val buf: StringBuilder = StringBuilder()
  
  private def advance(char: Character): Unit = char.char match
    case '\n' =>
      lastLine += 1
      lastCol = 0
    case _ =>
      lastCol += 1
  
  def next()(using Log): Character throws CodlParseError =
    if finished then throw IllegalStateException("Attempted to read past the end of the stream")
    in.read() match
      case -1 =>
        finished = true
        Character.End
      case '\r' =>
        requireCr match
          case None        => requireCr = Some(true)
          case Some(false) => throw CodlParseError(lastLine, lastCol, CarriageReturnMismatch(false))
          case Some(true)  => ()
        
        if in.read() != '\n' then throw CodlParseError(lastLine, lastCol, UnexpectedCarriageReturn)
        
        Character('\n', lastLine, lastCol).tap(advance)
      
      case '\n' =>
        requireCr match
          case Some(true)  => throw CodlParseError(lastLine, lastCol, CarriageReturnMismatch(true))
          case None        => requireCr = Some(false)
          case Some(false) => ()
        
        Character('\n', lastLine, lastCol).tap(advance)
      
      case ch =>
        Character(ch, lastLine, lastCol).tap(advance)
  
  def start(): (Int, Int) = (startLine, startCol)
  def get(): Text = buf.toString.show.tap(buf.clear().waive)
  
  def put(char: Character): Unit =
    if buf.isEmpty then
      startLine = char.line
      startCol = char.column

    buf.append(char.char)

sealed trait CodlError extends Exception

object CodlParseError:
  given Show[Issue] =
    case UnexpectedCarriageReturn =>
      t"TODO"
    case CarriageReturnMismatch(true) =>
      t"TODO"
    case CarriageReturnMismatch(false) =>
      t"TODO"
    case UnevenIndent(initial, indent) =>
      t"TODO"
    case IndentAfterComment =>
      t"TODO"
    case SurplusIndent =>
      t"TODO"
    case InsufficientIndent =>
      t"TODO"
  
  enum Issue:
    case UnexpectedCarriageReturn
    case CarriageReturnMismatch(required: Boolean)
    case UnevenIndent(initial: Int, indent: Int)
    case IndentAfterComment, SurplusIndent, InsufficientIndent

export CodlParseError.Issue.{UnevenIndent, IndentAfterComment, SurplusIndent, InsufficientIndent}

case class BinaryError(expectation: Text, pos: Int)
extends Exception(s"expected $expectation at position $pos")

case class CodlParseError(line: Int, col: Int, issue: CodlParseError.Issue)
extends Error(err"could not parse CoDL document at $line:$col: ${issue.show}"), CodlError

object CodlValidationError:
  enum Issue:
    case MissingKey(key: Maybe[Text])
    case DuplicateKey(key: Text, firstPos: Int)
    case SurplusParams
    case RequiredAfterOptional
    case InvalidKey(key: Maybe[Text])
    case DuplicateId(id: Text)

case class CodlValidationError(word: Maybe[Text], issue: CodlValidationError.Issue)
extends Error(err"the CoDL document did not conform to the schema at $word because $issue"), CodlError

case class MultipleIdentifiersError(key: Text)
extends Exception(s"multiple parameters of $key have been marked as identifiers")

case class MissingValueError(key: Text)
extends Error(err"the key $key does not exist in the CoDL document")

case class MissingIndexValueError(index: Int)
extends Error(err"the index $index does not exist in the CoDL document")

enum Token:
  case Newline(n: Int)
  case Tab(n: Int)
  case Word(text: Text, line: Int, col: Int)
  case Block(text: Text, line: Int, col: Int)
  case Comment(text: Text, line: Int, col: Int)
  case Remark(text: Text, line: Int, col: Int)
  case Blank(count: Int)

  override def toString(): String = this match
    case Word(t, l, c)    => (t"[$t:$l:$c]").s
    case Tab(n)           => t"==>".s
    case Newline(n)       => (t"{"+(t"-"*(-n))+(t"+"*n)+t"}").s
    case Block(t, l, c)   => t"#[bl[${t}]]#".s
    case Comment(t, l, c) => t"~$t:$l:$c~".s
    case Remark(t, l, c)  => t"^$t:$l:$c^".s
    case Blank(c)         => (t"\\n"*c).s

case class CodlStream(margin: Int, tokens: LazyList[Token]):
  override def toString: String = s"(${margin}) ${tokens.mkString(" · ")}"

def tokenize(in: Reader)(using Log): CodlStream throws CodlParseError =
  val reader = PositionReader(in)

  enum State:
    case Word, Hash, Comment, Indent, Space, Tab, Margin
  
  import State.*

  @tailrec
  def cue(): Character = reader.next() match
    case ch if ch.char == '\n' || ch.char == ' ' => cue()
    case ch                                      => ch
  
  val first: Character = cue()
  val margin: Int = first.column

  def istream(char: Character, state: State = Indent, indent: Int = margin): LazyList[Token] =
    stream(char, state, indent)
  
  @tailrec
  def stream(char: Character, state: State = Indent, indent: Int = margin): LazyList[Token] =
    Log.fine(t"stream('${char.char}', $state, $indent)")
    inline def recur(state: State, indent: Int = indent): LazyList[Token] = stream(reader.next(), state, indent)
    inline def irecur(state: State, indent: Int = indent): LazyList[Token] = istream(reader.next(), state, indent)
    inline def diff: Int = char.column - indent

    def token(): Token = state match
      case Comment => Token.Comment(reader.get(), reader.start()(0), reader.start()(1))
      case Margin  => Token.Block(reader.get().drop(1, Rtl), reader.start()(0), reader.start()(1))
      case Word    => Token.Word(reader.get(), reader.start()(0), reader.start()(1))
      case _       => ???

    def put(next: State, stop: Boolean = false): LazyList[Token] = token() #:: irecur(next)
    
    inline def consume(next: State): LazyList[Token] =
      reader.put(char)
      recur(next)

    inline def block(): LazyList[Token] =
      if char.char != ' ' && diff < 4 then token() #:: istream(char)
      else
        if char.char != ' ' || diff >= 4 then reader.put(char)
        recur(Margin)
    
    inline def newline(next: State): LazyList[Token] =
      if diff > 4 then throw CodlParseError(char.line, char.column, CodlParseError.Issue.SurplusIndent)
      if diff%2 != 0 then throw CodlParseError(char.line, char.column, CodlParseError.Issue.UnevenIndent(margin, char.column))
      Token.Newline(diff) #:: irecur(next, indent = char.column)

    char.char match
      case _ if char == End => state match
        case Indent    => LazyList()
        case Space     => LazyList()
        case Tab       => LazyList()
        case Hash      => LazyList()
        case Comment   => LazyList(token())
        case Word      => LazyList(token())
        case Margin    => LazyList(token())
      
      case '\n' => state match
        case Word | Comment => put(Indent)
        case Margin         => block()
        case _              => recur(Indent)
      
      case ' ' => state match
        case Space | Tab => recur(Tab)
        case Indent      => recur(Indent)
        case Word        => put(Space)
        case Comment     => consume(Comment)
        case Margin      => block()
        case Hash        => recur(Comment)
      
      case '#' => state match
        case Space     => recur(Comment)
        case Tab       => recur(Comment)
        case Word      => consume(Word)
        case Indent    => if diff == 4 then recur(Margin) else newline(Hash)
        case Comment   => consume(Comment)
        case Margin    => block()
        case Hash      => ??? //stream(next(), Comment, indent)
      
      case ch => state match
        case Space   => consume(Word)
        case Tab     => consume(Word)
        case Word    => consume(Word)
        case Comment => consume(Comment)
        case Indent  => reader.put(char); if diff == 4 then recur(Margin) else newline(Word)
        case Margin  => block()
        
        case Hash => char.char match
          case '!' if first.line == 0 && first.column == 1 => consume(Comment)
          case ch                                          => consume(Comment)

  CodlStream(first.column, stream(first).tail)


  // def indirect(cur: Character, state: State, indent: Int)(using Log)
  //             : LazyList[Token] throws CodlParseError =
  //   stream(cur, pos, state, indent, start)

  // @tailrec
  // final def stream(char: Character = Character(in.read()), pos: Pos = Pos(0, 0), state: State = Indent(0, 0),
  //                      indent: Int = 0, start: Pos = Pos(0, 0))(using Log)
  //                 : LazyList[Token] throws CodlParseError =
  //   Log.info(t"stream('${char.char}', ${pos.line}:${pos.col}, $state, $indent, ${start.line}:${start.col})")
  //   def take() = buf.append(char.char)
  //   def word() = Token.Word(get(), start)
  //   def comment() = Token.Comment(get(), start)
  //   def block() = Token.Block(get(), start)
    
  //   def startline(n: Int): Int throws CodlParseError =
  //     // if (n - indent)%2 == 1 then throw CodlParseError(pos, UnevenIndent(margin, n))
  //     // if (n - indent)/2 > 2 then throw CodlParseError(pos, SurplusIndent)
  //     n - indent

  //   inline def recur(state: State, pos: Pos = pos.adv, indent: Int = indent, start: Pos = start) =
  //     stream(Character(in.read()), pos, state, indent, start)
    
  //   inline def irecur(state: State, pos: Pos = pos.adv, indent: Int = indent, start: Pos = start) =
  //     indirect(Character(in.read()), pos, state, indent, start)
    
  //   char match
  //     case End  => state match
  //       case Indent(_, _) | Space(_) | Hash => LazyList()
  //       case Comment                        => LazyList(comment())
  //       case Word                           => LazyList(word())
  //       case Margin(n)                      => LazyList(block())
      
  //     case '\n' =>
  //       state match
  //         case Space(n)     => recur(Indent(0, 0), pos.newline)
  //         case Word         => word() #:: irecur(Indent(0, 0), pos.newline)
  //         case Indent(l, n) => recur(Indent(l + 1, 0), pos.newline)
  //         case Comment      => comment() #:: irecur(Indent(0, 0), pos.newline)
  //         case Margin(n)    => if n > indent + 4 then take(); recur(Margin(0), pos.newline)
  //         case Hash         => recur(Indent(0, 0), pos.newline)
      
  //     case ' ' =>
  //       state match
  //         case Space(n)     => recur(Space(n + 1))
  //         case Word         => word() #:: irecur(Space(1))
  //         case Indent(l, n) => recur(Indent(l, n + 1))
  //         case Comment      => take(); irecur(Comment)
  //         case Margin(n)    => if n > indent + 4 then take(); recur(Margin(n + 1))
  //         case Hash         => recur(Comment)
      
  //     case '#' =>
  //       state match
  //         case Space(n)     => recur(Comment)
  //         case Word         => take(); recur(Word)
  //         case Indent(l, n) => Token.Depth(startline(n)) #:: irecur(Hash, indent = n)
  //         case Comment      => take(); recur(Comment)
  //         case Margin(n)    => if n > indent + 4 then take(); recur(Indent(0, indent - n))
  //         case Hash         => ??? //stream(next(), Comment, indent)
      
  //     case char =>
  //       state match
  //         case Space(n)     => take(); recur(Word, start = pos)
  //         case Word         => take(); recur(Word)
  //         case Indent(l, n) => take(); Token.Depth(startline(n)) #:: irecur(Word, indent = n, start = pos)
  //         case Comment      => take(); recur(Comment)
          
  //         case Margin(n) =>
  //           if n >= indent + 4 then take()
  //           Token.Depth(startline(n)) #:: irecur(Indent(0, indent - n))
          
  //         case Hash => char match
  //           case '!' if pos == Pos(0, 1) => take(); recur(Comment)
  //           case _                       => ???

//   @tailrec
//   final def stream(cur: Character = read(), pos: Pos = Pos(0, 0), space: Boolean = true): LazyList[Token.Word] =
//     cur match
//       case End        => if buf.isEmpty then LazyList() else LazyList(word(pos))
//       case ' '        => if !space then word(pos) #:: indirect(read(), pos.adv)
//                          else stream(read(), pos.adv)
//       case '\n'       => if !space then word(pos) #:: indirect(read(), pos.crlf)
//                          else stream(read(), pos.crlf)
//       case other      => buf.append(other.char)
//                          stream(read(), pos.adv, false)

// object Codl:
//   val out = java.io.PrintStream(java.io.FileOutputStream(java.io.FileDescriptor.out))

//   enum State:
//     case Start, Comment, Params, Multiline, LastParam

//   def parse(in: Reader, schema: ListMap[Maybe[Text], Schema])(using Log): Doc throws CodlParseError | AggregateError | CodlValidationError =
//     val buf: StringBuilder = StringBuilder()

//     @tailrec
//     def trees(state: State, stream: LazyList[Token.Word], stack: List[Node], margin: Int, line: Int, end: Int,
//                   schemata: List[ListMap[Maybe[Text], Schema]])
//              : Doc throws CodlParseError | CodlValidationError =
//       Log.info(s"trees($state, ${stream.map(_.text).join(t"", t"…", t"!")}, ${stack.init.map(_.debug).join(t"/")}, mgn=$margin ln=$line end=$end)")
//       val level: Int = stack.length - 1
//       val indent: Int = level*2 + margin

//       def squish: List[Node] = state match
//         case State.LastParam | State.Multiline =>
//           val skey -> _ = schemata.head.head
//           stack match
//             case Node(Data(key, children, layout), meta) :: tail =>
//               skey.option match
//                 case None =>
//                   Node(Data(key, Node(Data(buf.toString.show)) :: children, layout), meta) :: tail
//                 case Some(skey) =>
//                   Node(Data(key, Node(Data(skey, List(Node(Data(buf.toString.show))))) :: children, layout), meta) :: tail
//             case _ =>
//               throw Mistake("Should never match")
//         case _ =>
//           ???

//       stream match
//         case Token.Word(word, pos) #:: rest =>
//           Log.info(t"word: $word ${pos.line}:${pos.col}")
//           // Right at the start
          
//           if margin < 0 then trees(State.Start, stream, stack, pos.col, line, end, schemata)
//           // Continuing the same line
//           else if pos.line == line then
//             val gap = pos.col - end
            
//             stack.head match
//               case Node(Data(key, children, layout), meta) =>
//                 val skey -> schema = schemata.head.headOption.getOrElse:
//                   throw CodlValidationError(key, CodlValidationError.Issue.SurplusParams)

//                 val head2 = skey.option.fold(Data(word))(Data(_, List(Node(Data(word)))))
//                 val stack2 = Node(Data(key, Node(head2) :: children, layout), meta) :: stack.tail
                
//                 val schemata2 =
//                   if schema.multiplicity.variadic then schemata else schemata.head.tail :: schemata.tail
                
//                 schema.multiplicity match
//                   case Multiplicity.Joined =>
//                     Log.info(t"Joined branch")
//                     if state == State.LastParam then for i <- 0 until gap do buf.append(' ')
//                     buf.append(word.s)
//                     trees(State.LastParam, rest, stack, margin, pos.line, pos.col + word.length, schemata)
                  
//                   case Multiplicity.AtLeastOne | Multiplicity.Many =>
//                     Log.info(t"variadic")
                    
//                     if state != State.Multiline
//                     then trees(State.Params, rest, stack2, margin, pos.line, pos.col + word.length, schemata2)
//                     else
//                       for i <- 0 until gap do buf.append(' ')
//                       buf.append(word.s)
//                       trees(State.Multiline, rest, stack, margin, pos.line, pos.col + word.length, schemata2)
                  
//                   case Multiplicity.One | Multiplicity.Optional | Multiplicity.Unique =>
//                     Log.info(t"unitary")
//                     trees(State.Params, rest, stack2, margin, pos.line, pos.col + word.length, schemata2)
              
//               case _ =>
//                 throw Mistake("impossible")

          
//           // Starting a new line
//           else state match
//             case State.LastParam =>
//               Log.info("last param")
//               trees(State.Params, stream, squish, margin, line, end, schemata.tail)
//             case State.Multiline if pos.col - indent >= 2 =>
//               Log.info("multiline word")
//               buf.append('\n')
//               for i <- 0 until (pos.col - indent - 2) do buf.append(' ')
//               buf.append(word.s)
//               trees(State.Multiline, rest, stack, margin, pos.line, pos.col + word.length, schemata)
//             case State.Multiline =>
//               trees(State.Params, stream, squish, margin, pos.line, pos.col, schemata)
//             case _ => 
//               (pos.col - indent) match
                
//                 // Line is uneven
//                 case i if i%2 != 0 =>
//                   throw CodlParseError(line, 0, UnevenIndent(margin, indent))
                
//                 // A new peer element
//                 case 0 =>
//                   if word.startsWith(t"#") then
//                     buf.clear()
//                     buf.append(word.drop(1))
//                     trees(State.Comment, rest, stack, margin, line, end, schemata)
//                   else
//                     val subschemas = schemata.headOption.flatMap(_.get(word)).getOrElse(Schema.Freeform).subschemas
//                     val stack2 = Node(Data(word)) :: stack
//                     trees(State.Params, rest, stack2, margin, pos.line, pos.col + word.length, subschemas :: schemata)
                
//                 // Multiline content
//                 case 2 =>
//                   state match
//                     // Continuation of a multiline content
//                     case State.Multiline =>
//                       Log.info("continuation")
//                       buf.append('\n')
//                       buf.append(word.s)
//                       trees(State.Multiline, rest, stack, margin, pos.line, pos.col + word.length, schemata)
                    
//                     // The start of a new multiline parameter
//                     case State.Params | State.LastParam =>
//                       Log.info(t"New multiline parameter")
//                       buf.clear()
//                       buf.append(word.s)
//                       trees(State.Multiline, rest, stack, margin, pos.line, pos.col + word.length, schemata)
                    
//                     case state =>
//                       unsafely(throw new Exception("Got state: "+state))
    
//                 // Too much indentation
//                 case i if i > 2 =>
//                   Log.info(t"deep indent")
//                   buf.clear()
//                   buf.append(word)
//                   trees(State.Multiline, rest, stack, margin, pos.line, pos.col + word.length, schemata)
//                 // Some level of unindentation
//                 case i =>
//                   stack match
//                     case node :: Node(Data(key, children, layout), meta) :: tail =>
//                       Log.info(s"Pre-close = ${node.debug}")
//                       Log.info(s"children = $children")
//                       Log.info("")
//                       trees(state, stream, Node(Data(key, node.close :: children, layout), meta) :: tail, margin, line, end, schemata.tail)
//                     case _ =>
//                       throw Mistake("Should never match")
        
//         case _ => stack match
//           case Nil =>
//             Doc(Nil, 0)
          
//           case Node(Data(t"", children, _), _) :: Nil =>
//             Doc(children.reverse, margin.max(0))
          
//           case node :: Node(Data(key, children, layout), meta) :: tail =>
//             state match
//               case State.LastParam =>
//                 trees(State.Params, stream, squish, margin, line, end, schemata.tail)
//               case State.Multiline =>
//                 trees(State.Params, stream, squish, margin, line, end, schemata.tail)
//               case _ =>
//                 val head2 = Node(Data(key, if node.empty then children else node.close :: children, layout), meta)
//                 trees(state, stream, head2 :: tail, margin, line, end, schemata)
          
//           case _ =>
//             throw Mistake("Should never match")
          
    
//     trees(State.Start, Tokenizer(in).stream(), List(Node(Data(t""))), -1, -1, 0, List(schema))


object Node:
  given DebugString[Node] = _.data.option.fold(t"!") { data => t"${data.key}[${data.children.map(_.debug).join(t",")}]" }
  val empty: Node = Node()
  def apply(key: Text)(child: Node*): Node = Node(Data(key, child.to(List)))

case class Node(data: Maybe[Data] = Unset, meta: Maybe[Meta] = Unset) extends Dynamic:
  def add(node: Data): Node = copy(data = data.otherwise(???).copy())
  def empty: Boolean = data == Unset && meta == Unset

  def close: Node = data match
    case Unset                       => Node(Unset, meta)
    case Data(key, children, layout) => Node(Data(key, children.reverse, layout), meta)
  
  def selectDynamic(key: String): List[Data] throws MissingValueError =
    data.option.getOrElse(throw MissingValueError(key.show)).selectDynamic(key)
  
  def applyDynamic(key: String)(idx: Int = 0): Data throws MissingValueError = selectDynamic(key)(idx)

// // FIXME: Remove this later
// extension [K, V](map: Map[K, List[V]])
//   def plus(key: K, value: V): Map[K, List[V]] =
//     map.updated(key, map.get(key).fold(List(value))(value :: _))

trait Indexed extends Dynamic:
  def children: List[Node]
  private lazy val index: Map[Text, List[Data]] =
    children.map(_.data).sift[Data].foldLeft(Map[Text, List[Data]]()): (acc, data) =>
      acc.plus(data.key, data)
    .view.mapValues(_.reverse).to(Map)

  def apply(index: Int): Node throws MissingIndexValueError =
    try children(index) catch case err: IndexOutOfBoundsException => throw MissingIndexValueError(index)

  def selectDynamic(key: String): List[Data] throws MissingValueError =
    index.get(key.show).getOrElse(throw MissingValueError(key.show))
  
  def applyDynamic(key: String)(idx: Int = 0): Data throws MissingValueError = selectDynamic(key)(idx)
  
case class Data(key: Text, children: List[Node] = Nil, layout: Maybe[Layout] = Unset) extends Indexed
case class Meta(blank: Int, comments: List[Text], remark: Maybe[Text]/*, grid: Grid*/)
case class Layout(params: Int, multiline: Boolean)

object Doc:
  def apply(node: Node*): Doc = Doc(node.to(List), 0)

case class Doc(children: List[Node], margin: Int) extends Indexed

object Schema:
  object Freeform extends Schema(ListMap(Unset -> Schema(ListMap(), Multiplicity.Many)), Multiplicity.Many)

case class Schema(subschemas: ListMap[Maybe[Text], Schema] = ListMap(), multiplicity: Multiplicity = Multiplicity.One):
  // def parse(text: Text)(using Log): Doc throws AggregateError | CodlParseError | CodlValidationError =
  //   Codl.parse(StringReader(text.s), subschemas)
  
  def apply(key: Maybe[Text]): Schema = subschemas.get(key).getOrElse(Schema.Freeform)

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
  
  def unique: Boolean = !repeatable

// enum Tweak:
//   case HalfIndent, HalfOutdent

// case class Recovery(suppliedFixes: List[Int]):
//   var fixes: List[Int] = suppliedFixes
//   var failures: List[CodlError] = Nil
//   var appliedFixes: List[Int] = Nil
//   var alternatives: Option[Int] = None

// case class Raise(error: CodlError):
//   def fix[T, F](defaultFix: F, alternatives: F*)(handle: F => T)(using recovery: Recovery): T throws AggregateError =
//     throw AggregateError(List(error))
//     /*
//     recovery.failures ::= error
//     val fix: F =
//       if recovery.suppliedFixes.length > recovery.failures.size
//       then
//         alternatives(recovery.suppliedFixes(recovery.failures.size))
//       else
//         if recovery.suppliedFixes.length == recovery.failures.size
//         then
//           recovery.alternatives = Some(alternatives.size)

//         defaultFix
    
//     recovery.appliedFixes ::= alternatives.indexOf(fix)
//     handle(fix)*/
  
//   def recover[T](value: T)(using Recovery): T throws AggregateError = fix(())((_: Any) => value)

// def raise[T, Fix](error: => CodlError) = Raise(error)

// def attempt[T](fn: Recovery ?=> T): T throws AggregateError =
  
//   @tailrec
//   def recur(recovery: Recovery): T =
//     val result = fn(using recovery)
    
//     recovery.alternatives match
//       case None =>
//         if recovery.failures.length > 0 then throw AggregateError(recovery.failures)
//         result
//       case Some(n) =>
//         val bestFix = (0 to n).minBy: index =>
//           val trialRecovery = Recovery(index :: recovery.fixes)
//           fn(using trialRecovery)
//           trialRecovery.failures.length
        
//         recur(Recovery(bestFix :: recovery.suppliedFixes))
  
//   recur(Recovery(Nil))
      
case class AggregateError(errors: List[CodlError])
extends Error(err"aggregation of errors: ${errors.map(_.toString.show).join.s}")

// // case class MissingRefError(ref: Text, other: Any) extends Error(err"reference $ref was not found in ${other.toString}")

// // case class InvalidFormatError() extends Error(err"the CoDL data was not in the expected format")


// case class Grid(ranges: TreeMap[Int, Vector[Int]] = TreeMap()):
//   def apply(line: Int): Vector[Int] = ranges.maxBefore(line + 1).fold(Vector())(_(1))
  
//   def add(line: Int, index: Int, column: Int): Grid =
//     ranges.maxBefore(line + 1) match
//       case None =>
//         Grid(ranges.updated(line, Vector.fill(index)(0) :+ column))
      
//       case Some((rLine, cols)) =>
//         if index >= cols.length then Grid(ranges.updated(rLine, cols.padTo(index, 0) :+ column))
//         else if cols(index) == column then this
//         else Grid(ranges.updated(line, cols.updated(index, column)))
// object Bin:
//   def write(out: Writer, number: Int): Unit = out.write((number + 32).toChar)

//   def write(out: Writer, text: Text): Unit =
//     write(out, text.length)
//     out.write(text.s)

//   // def write(out: Writer, doc: CodlDoc): Unit throws CodlValidationError =
//   //   out.write("\u00b1\u00c0\u00d1")
//   //   write(out, Subschema(t"", doc.schema.subschemas, Multiplicity.One), doc.children)

//   // private def write(out: Writer, schema: Subschema, structs: List[Struct])
//   //                  : Unit throws CodlValidationError =
//   //   write(out, structs.length)
    
//   //   structs.foreach:
//   //     case Space(length, comment) =>
//   //       ()

//   //     case Branch(key, _, children, leading, trailing) =>
//   //       val idx: Int = schema.keyMap(key)
//   //       write(out, idx)
//   //       write(out, 0)
//   //       write(out, schema.subschemas(idx), children)
      
//   //     case Param(key, values, _, _, _, _) =>
//   //       val idx: Int = schema.keyMap(key)
//   //       write(out, idx)
//   //       write(out, values.length)
//   //       values.foreach(write(out, _))
  
//   // def readDoc(schema: SchemaDoc, reader: Reader): CodlDoc throws BinaryError =
//   //   if reader.read() != '\u00b1' || reader.read() != '\u00c0' || reader.read() != '\u00d1'
//   //   then throw BinaryError(t"header 0xb1c0d1", 0)
    
//   //   def recur(schemas: List[Subschema]): List[Struct] =
//   //     List.range(0, readNumber(reader)).map:
//   //       idx =>
//   //         val idx = readNumber(reader)
//   //         val subschema = schemas(idx)
          
//   //         Bin.readNumber(reader) match
//   //           case 0 => Branch(subschema.key, 0, recur(subschema.subschemas), None, None)
//   //           case n => val values = List.range(0, n).map { _ => readText(reader) }
//   //                     val multiline = values.exists { text => text.contains(' ') || text.contains('\n') }
//   //                     Param(subschema.key, values, multiline, 0, None, None)
    
//   //   CodlDoc(SchemaDoc(schema.subschemas), recur(schema.subschemas), 0)

//   def readNumber(in: Reader): Int = in.read - 32

//   def readText(in: Reader, length: Int = -1): Text =
//     val buf = new Array[Char](if length == -1 then readNumber(in) else length)
//     in.read(buf)
//     String(buf).show
  
// object Serializer extends Derivation[Serializer]:
//   def join[T](ctx: CaseClass[Serializer, T]): Serializer[T] = value =>
//     val data: List[(Multiplicity, List[Data])] = ctx.params.to(List).map: param =>
//       param.typeclass.multiplicity -> param.typeclass(param.deref(value)).map(_.relabel(param.label.show))
    
//     val params: Map[Maybe[Text], List[Text]] = data.takeWhile:
//       case (Multiplicity.One, List(key -> List(value))) if !value.contains(' ') && !value.contains('\n') => true
//       case _                                                                                                       => false
//     .flatMap(_(1)).to(Map)
    
//     val children: List[Point] = data.drop(params.size).flatMap(_(1)).map:
//       case v@Value(key, value) => v.promote
//       case point               => point
    
//     List(Node(ctx.typeInfo.short.show, params, IArray(children*)))


//   def split[T](ctx: SealedTrait[Serializer, T]): Serializer[T] = ???

//   given Serializer[Text] = value => List(Value(Unset, List(value.trim)))
//   given Serializer[Int] = value => List(Value(Unset, List(value.show)))
  
//   given [T: Serializer]: Serializer[Seq[T]] with
//     def apply(value: Seq[T]): List[Data] = value.flatMap(summon[Serializer[T]](_)).to(List)
//     override def multiplicity: Multiplicity = Multiplicity.Many

// trait Serializer[-T]:
//   def apply(value: T): List[Data]
//   def multiplicity: Multiplicity = Multiplicity.One
//   def contraMap[S](fn: S => T): Serializer[S] = apply.compose(fn)(_)

// object Deserializer extends Derivation[Deserializer]:
//   def join[T](ctx: CaseClass[Deserializer, T]): Deserializer[T] = value =>
//     unsafely(Some:
//       value.head match
//         case node: Node =>
//           ctx.construct: param =>
//             param.typeclass(node.selectDynamic(param.label)).getOrElse(throw Exception(s"${param.label} not extractable from ${node.index} using ${param.typeclass}"))
//     )
    
//   def split[T](ctx: SealedTrait[Deserializer, T]): Deserializer[T] = ???

//   given Deserializer[Text] =
//     _.map:
//       case value: Text => value
//       KeyValue(key, values) => Some(values)
//       case _                     => None
//     .headOption

//   given [T: [S] =>> Unapply[Text, S]]: Deserializer[T] = summon[Deserializer[Text]].flatMap(As.unapply[T](_))

//   given [T: Deserializer]: Deserializer[List[T]] = data => Some:
//     data.flatMap: data =>
//       summon[Deserializer[T]](List(data))
    

// trait Deserializer[+T]:
//   def apply(data: List[Data]): Option[List[T]]
//   def map[S](fn: T => S): Deserializer[S] = apply(_).map(_.map(fn))
//   def flatMap[S](fn: T => Option[S]): Deserializer[S] = apply(_).map(_.flatMap(fn))

// object SchemaGen extends Derivation[SchemaGen]:
//   def join[T](ctx: CaseClass[SchemaGen, T]): SchemaGen[T] = () =>
//     val children = ctx.params.to(List).map:
//       param => Subschema(param.label.show, param.typeclass.schema().subschemas, Multiplicity.One)
    
//     SchemaDoc(children)
  
//   def split[T](ctx: SealedTrait[SchemaGen, T]): SchemaGen[T] = () => ???

//   given SchemaGen[Text] = () => SchemaDoc(List(Value))
//   given SchemaGen[Int] = () => SchemaDoc(List(Value))
//   given [T: SchemaGen]: SchemaGen[Seq[T]] = () =>
//     val schema = summon[SchemaGen[T]].schema()
//     schema.copy(subschemas = schema.subschemas.map(_.copy(multiplicity = Multiplicity.Many)))

// trait SchemaGen[-T]:
//   def schema(): SchemaDoc

// extension [T: SchemaGen: Serializer](value: T) def codl: Doc =
//   Doc(summon[SchemaGen[T]].schema(), IArray(summon[Serializer[T]](value).flatMap(_.allChildren)*), 0)

// object Subschema:
//   def parse(data: List[Raw]): SchemaDoc throws MultipleIdentifiersError = 
//     def parseNodes(data: List[Raw]): List[Subschema] throws MultipleIdentifiersError =
//       data.sift[Raw.Data].map: node =>
//         val multiplicity = Multiplicity.parse(node.key.last)
//         val key = if multiplicity == Multiplicity.One then node.key else node.key.drop(1, Rtl)
        
//         val paramSchemas = node.data.sift[Tokenizer.Token.Word].map: word =>
//           val multiplicity = Multiplicity.parse(word.text.last)
//           val key = if multiplicity == Multiplicity.One then word.text else word.text.drop(1, Rtl)

//           Subschema(key, List(Value), multiplicity)
        
//         if paramSchemas.count(_.multiplicity == Multiplicity.Unique) > 1
//         then throw MultipleIdentifiersError(key)
        
//         Subschema(key, paramSchemas ++ parseNodes(node.children), multiplicity)
    
//     SchemaDoc(parseNodes(data))

// sealed trait Schema:
//   lazy val keyMap: Map[Maybe[Text], Int] = subschemas.zipWithIndex.map(_.key -> _).to(Map)
//   def subschemas: List[Subschema]
//   def freeform = keyMap.contains(Unset)
//   def required: List[Text] = subschemas.filter(_.multiplicity.required).map(_.key).sift[Text]
//   def unique: List[Text] = subschemas.filter(_.multiplicity.unique).map(_.key).sift[Text]

//   def apply(key: Text): Schema throws CodlValidationError =
//     if freeform then SchemaDoc.Freeform else keyMap.get(key).map(subschemas(_)).getOrElse:
//       throw CodlValidationError(key, CodlValidationError.Issue.InvalidKey(key))

// object Value extends Subschema(t"", Nil, Multiplicity.One)

// object SchemaDoc:
//   val Freeform = SchemaDoc(List(Subschema(Unset, List(Subschema(
//       Unset, Nil, Multiplicity.Many)), Multiplicity.Many)))

//   def parse(text: Text)(using Log): SchemaDoc throws AggregateError | MultipleIdentifiersError =
//     Subschema.parse(Codl.tokenize(StringReader(text.s)).stream.to(List))

// case class SchemaDoc(subschemas: List[Subschema]) extends Schema:
//   def parse(text: Text)(using Log): Doc throws AggregateError = Codl.parse(text, this)

// case class Subschema(key: Maybe[Text], subschemas: List[Subschema], multiplicity: Multiplicity)
// extends Schema:
//   def identifier: Option[Subschema] = subschemas.find(_.multiplicity == Multiplicity.Unique)

// object Multiplicity:
//   def parse(symbol: Char): Multiplicity = symbol match
//     case '+' => Multiplicity.AtLeastOne
//     case '?' => Multiplicity.Optional
//     case '*' => Multiplicity.Many
//     case '&' => Multiplicity.Joined
//     case '!' => Multiplicity.Unique
//     case _   => Multiplicity.One

// enum Multiplicity:
//   case One, AtLeastOne, Optional, Many, Joined, Unique

//   def required: Boolean = this match
//     case One | Unique | AtLeastOne => true
//     case Optional | Many | Joined  => false

//   def variadic: Boolean = this match
//     case Joined | AtLeastOne | Many => true
//     case Optional | Unique | One    => false
  
//   def repeatable: Boolean = this match
//     case AtLeastOne | Many => true
//     case _                 => false
  
//   def unique: Boolean = !repeatable

// object Doc:
//   given Show[Doc] = _.render

// case class Doc(schema: SchemaDoc, children: IArray[Point], indentation: Int):
//   def render =
//     val buf: StringBuilder = StringBuilder()
//     children.foreach(_.render(indentation, buf))
//     buf.toString.show
  
//   def as[T: Deserializer](using Log): T throws InvalidFormatError =
//     summon[Deserializer[T]](Node(Unset, Map(), IArray(children*))).getOrElse:
//       throw InvalidFormatError()

// trait Point:
//   def children: IArray[Point]
//   def params: Map[Maybe[Text], List[Text]]
//   def apply(): Maybe[Text]
//   def render(indent: Int, buf: StringBuilder): Unit

// trait Data extends Point:
//   def relabel(key: Text): Data
//   def key: Maybe[Text]
//   def apply(): Maybe[Text] = key
//   def allChildren: IArray[Point]

// case class Gap(comments: List[Text], lines: Int) extends Point:
//   def children: IArray[Point] = IArray()
//   def params: Map[Maybe[Text], List[Text]] = Map()
//   def apply(): Maybe[Text] = Unset
  
//   def render(indent: Int, buf: StringBuilder): Unit =
//     comments.foreach: comment =>
//       for i <- 0 until indent do buf.append("  ")
//       buf.append('#')
//       buf.append(comment)
//       buf.append('\n')

//     for i <- 0 until lines do buf.append('\n')

//case class Node(key: Maybe[Text], params: Map[Maybe[Text], List[Text]] = Map(),
                    //children: IArray[Point] = IArray(), meta: Meta = Meta.empty)
//extends Data, Dynamic:

//   override def equals(that: Any): Boolean = that.matchable(using Unsafe) match
//     case that: Node =>
//       // val equalParams = params.size == that.params.size && params.indices.forall: i =>
//       //   params(i) == that.params(i)
      
//       val equalChildren = children.length == that.children.length && children.indices.forall: i =>
//         children(i) == that.children(i)
      
//       that.key == key && that.params == params && equalChildren && that.meta == meta
    
//     case _ =>
//       false

//     override def hashCode: Int =
//       val init = key.hashCode ^ meta.hashCode
//       children.foldLeft(params.foldLeft(init)(_ ^ _.hashCode))(_ ^ _.hashCode)

//   def relabel(key: Text): Node = copy(key = key)
//   def allChildren: IArray[Point] = IArray(params ++ children).to(Seq)*)

//   def selectDynamic(ref: String): List[Data] throws MissingRefError =
//     index.get(ref.show).map(_.to(List)).getOrElse:
//       if index.size == 1 && index.contains(Unset) then index(Unset).to(List)
//       else throw MissingRefError(ref.show, index)

//   def render(indent: Int, buf: StringBuilder): Unit =
//     meta.preComment.foreach: line =>
//       for i <- 0 until indent do buf.append(' ')
//       buf.append('#')
//       buf.append(line)
//       buf.append('\n')

//     for i <- 0 until indent do buf.append(' ')
    
//     buf.append:
//       key.option.fold("")(_.toString)
    
//     params.init.foreach: (k, v) =>
//       buf.append(' ')
//       buf.append(v)
    
//     meta.postComment.foreach: comment =>
//       for i <- 0 until meta.rightPadding do buf.append(' ')
//       buf.append("# ")
//       buf.append(comment)
    
//     params.lastOption.foreach: (key, content) =>
//       content.cut(t"\n").foreach: line =>
//         buf.append('\n')
//         for i <- 0 until (indent + 4) do buf.append(' ')
//         buf.append(line)
    
//     buf.append('\n')
    
//     children.foreach: child =>
//       child.render(indent + 2, buf)

// object Meta:
//   final val empty: Meta = Meta(Nil, 0, None)

// case class Meta(preComment: List[Text], rightPadding: Int, postComment: Option[Text])

//object Codl:
  // def parse(text: Text, schema: SchemaDoc = SchemaDoc.Freeform)(using Log): Doc throws AggregateError =
  //   parse(StringReader(text.s), schema)

  // def parse(reader: Reader, schema: SchemaDoc)(using Log): Doc throws AggregateError = attempt:
  //   def indirectParams(todo: List[Tokenizer.Token], schemas: List[Subschema], values: Map[Maybe[Text], List[Text]],
  //                          opt: Boolean, ids: List[Tokenizer.Token.Word])
  //                     : (Option[Subschema], Map[Maybe[Text], List[Text]], List[Tokenizer.Token.Word]) =
  //     parseParams(todo, schemas, values, opt, ids)

    // @tailrec
    // def parseParams(todo: List[Tokenizer.Token], schemas: List[Subschema], values: Map[Maybe[Text], List[Text]],
    //                     opt: Boolean, ids: List[Tokenizer.Token.Word])
    //                : (Option[Subschema], Map[Maybe[Text], List[Text]], List[Tokenizer.Token.Word]) =
    //   todo match
    //     case Nil | Tokenizer.Token.Padding(_, _) :: Nil =>
    //       (schemas.headOption, values, ids)
        
    //     case Tokenizer.Token.Padding(gap, _) :: (token@Tokenizer.Token.Word(word, _)) :: stillTodo => schemas match
    //       case Nil =>
    //         raise(CodlValidationError(word, CodlValidationError.Issue.SurplusParams)).recover:
    //           indirectParams(stillTodo, Nil, values, opt = false, ids)
          
    //       case subschema :: schemasTodo => subschema.multiplicity match
    //         case Multiplicity.One =>
    //           if opt then raise(CodlValidationError(word, CodlValidationError.Issue.RequiredAfterOptional)).recover(())
    //           parseParams(stillTodo, schemasTodo, values.plus(subschema.key, word), opt = false, ids)
            
    //         case Multiplicity.Unique =>
    //           if opt then raise(CodlValidationError(word, CodlValidationError.Issue.RequiredAfterOptional)).recover(())
              
    //           parseParams(stillTodo, schemasTodo, values.plus(subschema.key, word), opt = false, token :: ids)
            
    //         case Multiplicity.Joined =>
    //           val content = word+stillTodo.map(_.serialize).join
    //           parseParams(Nil, Nil, values.plus(subschema.key, content), opt = opt, ids)
            
    //         case Multiplicity.Optional =>
    //           parseParams(stillTodo, schemasTodo, values.plus(subschema.key, word), opt = true, ids)
            
    //         case Multiplicity.Many | Multiplicity.AtLeastOne =>
    //           parseParams(stillTodo, schemas, values.plus(subschema.key, word), opt = opt, ids)
        
    //     case _ =>
    //       throw Mistake("This case should be unreachable")

    // def validate(schema: Schema, data: List[Raw], params: Map[Maybe[Text], List[Text]]): IArray[Point] =
    //   def points(result: List[Point], data: List[Raw], missing: Set[Maybe[Text]], allIds: Map[Maybe[Text], Tokenizer.Token.Word], index: Map[Maybe[Text], List[Text | Node]]): IArray[Point] =
    //     data match
    //       case Nil =>
    //         missing.foreach: key =>
    //           raise(CodlValidationError(key, CodlValidationError.Issue.MissingKey(key))).recover(())
              
    //         IArray(result.reverse*)

    //       case Raw.Data(key, args, longLines, children, comment) :: tail =>
    //         val subschema = try schema(key) catch case error: CodlValidationError =>
    //           raise(error).recover(SchemaDoc.Freeform)
  
    //         val cParams = args.dropWhile(_.serialize != t"#").drop(2)
    //         val paramArgs = args.takeWhile(_.serialize != t"#")
            
    //         val commentPadding = paramArgs.lastOption match
    //           case Some(Tokenizer.Token.Padding(count, _)) => count
    //           case _                                       => 0
            
    //         val lineComment = if cParams.isEmpty then None else Some(cParams.map(_.serialize).join)
    //         val (last, parameters, ids) = parseParams(paramArgs, subschema.subschemas, Map(), false, Nil)

    //         val newIds = ids.foldLeft(allIds): (allIds, token) =>
    //           if allIds.contains(token.text)
    //           then raise(CodlValidationError(key, CodlValidationError.Issue.DuplicateId(token.text))).recover(allIds)
    //           else allIds.updated(token.text, token)
            
    //         val valueMap: Map[Maybe[Text], List[Text]] = last match
    //           case None =>
    //             if longLines.isEmpty then parameters else
    //               raise(CodlValidationError(key, CodlValidationError.Issue.SurplusParams)).recover(parameters)
    //           case Some(last) =>
    //             if longLines.isEmpty then parameters
    //             else parameters.plus(last.key, longLines.reverse.join(t"\n"))
            
    //         if schema.unique.contains(key) && index.contains(key)
    //         then raise(CodlValidationError(key, CodlValidationError.Issue.DuplicateKey(key, 0))).recover(())
            
    //         val meta = Meta(comment, commentPadding, lineComment)
    //         val node = Node(key, valueMap, validate(subschema, children, valueMap), meta)
            
    //         points(node :: result, tail, missing - key, newIds, index.updated(key, List(node)))
  
    //       case Raw.Blank(count, comments) :: tail =>
    //         points(Gap(comments, count) :: result, tail, missing, allIds, index)
      
    //   points(Nil, data, schema.required.to(Set) -- params.map(_(0)), Map(), params.mapValues(List(_)).to(Map))
      
    // val tokens = tokenize(reader)

    // Doc(schema, validate(schema, tokens.stream.to(List), Map()), tokens.indentation)



  // def tokenize(in: Reader)(using Log): RawStream throws AggregateError = attempt:
  //   def indirectTrees = trees

  //   @tailrec
  //   def trees(stream: LazyList[Tokenizer.Token], last: Int, focus: List[Raw],
  //                 stack: List[List[Raw]], comments: List[Text] = Nil, blanks: Int = 0,
  //                 multiline: Boolean = false, lineNo: Int = 0, lineStart: Int = 0)(using Log): List[Raw] =
  //     stream match
  //       case LazyList() =>
  //         if stack.isEmpty then focus.reverse else stack match
  //           case (Raw.Data(key, data, longLines, _, comment, grid) :: neck) :: tail =>
  //             val node = Raw.Data(key, data, longLines, focus.reverse, comment, grid)
  //             trees(stream, 0, node :: neck, tail, Nil, blanks)
            
  //           case (blank :: Raw.Data(key, data, longLines, _, comment, grid) :: neck) :: tail =>
  //             val node = Raw.Data(key, data, longLines, blank :: focus.reverse, comment, grid)
  //             trees(stream, 0, node :: neck, tail, Nil, blanks)
            
  //           case _ =>
  //             throw Mistake("This case should be unreachable")
        
  //       case Word(key, line, pos) #:: rest =>
  //         val count = pos - lineStart - initPrefix
  //         Log.info(t"$pos / $lineStart / $initPrefix / $count")
  //         if blanks > 1 then
  //           trees(stream, last, Raw.Blank(blanks - 1, comments) :: focus, stack, Nil, 0, false, lineNo, lineStart)
  //         else
  //           lazy val data = rest.takeWhile(!_.newline).to(List)
            
  //           val grid = data.map(_.position - pos)

  //           Log.info(grid.toList.toString.show)

  //           lazy val line = key+data.map(_.serialize).join
  //           lazy val surplus = rest.drop(data.length)
            
  //           def appendLine(text: Text): List[Raw] = focus match
  //             case Nil =>
  //               raise(CodlParseError(lineNo, 0, SurplusIndent)).recover(Nil)
              
  //             case Raw.Data(key, args, longLines, children, comment, grid) :: tail =>
  //               Raw.Data(key, args, text :: longLines, children, comment, grid) :: tail
              
  //             case _ =>
  //               throw Mistake("There should never be a Blank node here")
            
  //           if multiline && (count - initPrefix > last*2 + 4) then
  //             val prefix = t" "*(count - initPrefix - last*2 - 4)
  //             trees(surplus, last, appendLine(prefix+line), stack, comments, 0, true, lineNo, lineStart)
  //           else
  //             if count < initPrefix then
  //               raise(CodlParseError(lineNo, 0, CodlParseError.Issue.InsufficientIndent)).recover(())
             
  //             val level =
  //               if (count - initPrefix)%2 == 0 then (count - initPrefix)/2 else raise:
  //                 CodlParseError(lineNo, 0, UnevenIndent(initPrefix, count))
  //               .fix(Tweak.HalfOutdent, Tweak.HalfIndent):
  //                 case Tweak.HalfOutdent => (count - initPrefix)/2
  //                 case Tweak.HalfIndent  => (count - initPrefix)/2 + 1
              
  //             (level - last) match
  //               case 2 =>
  //                 trees(surplus, last, appendLine(line), stack, comments, 0, true, lineNo, lineStart)
                
  //               case 1 =>
  //                 if !comments.isEmpty then raise(CodlParseError(lineNo, 0, IndentAfterComment)).recover(Nil)
  //                 else trees(stream, level, Nil, focus :: stack, Nil, blanks, false, lineNo, lineStart)
                
  //               case 0 =>
  //                 key.head match
  //                   case '#' =>
  //                     trees(surplus, last, focus, stack, line.drop(1) :: comments, 0, false, lineNo, lineStart)
                  
  //                   case _ =>
  //                     val node = Raw.Data(key, data.map(_.text), Nil, Nil,
  //                         comments.reverse, Grid())
  //                     trees(surplus, level, node :: focus, stack, Nil, 0, false, lineNo, lineStart)
                
  //               case n if n > 2 =>
  //                 raise(CodlParseError(lineNo, 0, SurplusIndent)).recover:
  //                   indirectTrees(surplus, last, appendLine(line), stack, comments, 0, true, lineNo, lineStart)
                
  //               case n =>
  //                 stack match
  //                   case (Raw.Data(key, data, longLines, _, comment, grid) :: neck) :: tail =>
  //                     val node = Raw.Data(key, data, longLines, focus.reverse, comment, grid)
  //                     trees(stream, last - 1, node :: neck, tail, Nil, blanks, multiline, lineNo, lineStart)
                    
  //                   case Nil =>
  //                     raise(CodlParseError(lineNo, 0, InsufficientIndent)).recover:
  //                       indirectTrees(stream, last - 1, focus, stack, Nil, blanks, multiline, lineNo, lineStart)
  
  //                   case _ =>
  //                     throw Mistake("This case should be unreachable")
        
  //       case _ =>
  //         throw Mistake("This case should be unreachable")

  //   RawStream(trees(Tokenizer(in).stream(), 0, Nil, Nil), initPrefix)
