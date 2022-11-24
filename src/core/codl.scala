package cellulose

import java.io.*
import gossamer.*
import eucalyptus.*
import rudiments.*

given Realm(t"codl")

enum Token:
  case Indent, Peer
  case Outdent(n: Int)
  case Tab(n: Int)
  case Item(text: Text, line: Int, col: Int, block: Boolean = false)
  case Comment(text: Text, line: Int, col: Int)
  case Blank

  override def toString(): String = this match
    case Item(t, l, c, b) => s"[$t:$l:$c:$b]"
    case Tab(n)           => "==>"
    case Indent           => "-->"
    case Peer             => "<->"
    case Outdent(n)       => s"<-${n}-"
    case Comment(t, l, c) => t"~$t:$l:$c~".s
    case Blank            => "[    ]"


case class Proto(data: Maybe[PData] = Unset, meta: Maybe[Meta] = Unset):
  def close: Node = data.mfold(Node(Unset, meta)):
    case PData(key, children, layout, schema) =>
      Node(Data(key, children.reverse, layout, schema), meta.mmap { m => m.copy(comments = m.comments.reverse) })

  def schema: Maybe[Schema] = data.mmap(_.schema)
  def has(key: Maybe[Text]): Boolean = data.mmap(_.has(key)).otherwise(false)
  def key: Maybe[Text] = data.mmap(_.key)

case class PData(key: Text, children: List[Node] = Nil, layout: Layout = Layout.empty, schema: Schema = Schema.Free):
  def has(key: Maybe[Text]): Boolean = dictionary.contains(key)
  
  // FIXME: These need to go

  def paramCount: Int = layout.mfold(0)(_.params)
  private lazy val array: IArray[Data] = IArray.from(children.map(_.data).sift[Data])
  private lazy val dictionary: Map[Maybe[Text], List[Data]] =
    val data = children.map(_.data).sift[Data]
    
    val init: Map[Maybe[Text], List[Data]] = schema match
      case schema@Struct(_, _) =>
        array.take(paramCount).zipWithIndex.foldLeft(Map[Maybe[Text], List[Data]]()):
          case (acc, (param, idx)) => acc.plus(schema.param(idx).mmap(_.key), param)
      case Field(_, _) =>
        Map()
    
    data.drop(paramCount).foldLeft(init): (acc, data) =>
      acc.plus(data.key, data)
    .view.mapValues(_.reverse).to(Map)


object Codl:

  def parse(reader: Reader, schema: Schema = Schema.Free)(using Log)
           : Doc throws CodlParseError | CodlValidationError =
    val (margin, stream) = tokenize(reader)
    val baseSchema: Schema = schema
    @tailrec
    def recur(tokens: LazyList[Token], focus: Proto, peers: List[Node], stack: List[(Proto, List[Node])], lines: Int)
             : Doc =
      
      def schema: Schema = stack.headOption.fold(baseSchema)(_.head.schema.option.get)
      
      inline def go(tokens: LazyList[Token] = tokens.tail, focus: Proto = focus, peers: List[Node] = peers,
                        stack: List[(Proto, List[Node])] = stack, lines: Int = lines)
                   : Doc =
        recur(tokens, focus, peers, stack, lines)
      
      tokens match
        case head #:: tail => head match
          case Token.Peer => focus match
            case Proto(Unset, meta) =>
              go(focus = Proto(Unset, meta.otherwise(if lines == 0 then Unset else Meta(lines))))
            
            case Proto(data: PData, meta) =>
              val params = focus.data.mfold(0)(_.children.length)
              val layout2 = focus.data.mmap(_.layout).mfold(Layout(params, false))(_.copy(params = params))
              val data2 = focus.data.mmap(_.copy(layout = layout2))
              go(focus = Proto(), peers = focus.copy(data = data2).close :: peers)
          
          case Token.Indent =>
            val params = focus.data.mfold(0)(_.children.length)
            val layout2 = focus.data.mmap(_.layout).mfold(Layout(params, false))(_.copy(params = params))
            val data2 = focus.data.mmap(_.copy(layout = layout2))
            
            go(focus = Proto(), peers = Nil, stack = (focus.copy(data = data2) -> peers) :: stack)
          
          case Token.Outdent(n) => stack match
            case Nil =>
              go(LazyList())
            
            case (Proto(data: PData, meta) -> rest) :: stack2 =>
              val next = if n == 1 then Token.Peer else Token.Outdent(n - 1)
              val focus2 = Proto(data.copy(children = (focus.close :: peers)), meta)
              
              focus.schema.mfold(Nil)(_.requiredKeys).foreach: key =>
                if !focus.has(key) then throw CodlValidationError(focus.key, MissingKey(key))
              
              go(next #:: tail, focus = focus2, peers = rest, stack = stack2)
          
          case Token.Tab(n) =>
            go()
          
          case Token.Blank => focus.meta match
            case Unset            => go(lines = lines + 1)
            case Meta(l, _, _, _) => go(focus = Proto(), peers = focus.close :: peers, lines = lines + 1)
          
          case Token.Item(word, line, col, block) =>
            val meta2: Maybe[Meta] = focus.meta.otherwise(if lines == 0 then Unset else Meta(blank = lines))
            
            focus match
              case Proto(Unset, meta) =>
                val fschema = if schema == Schema.Free then schema else schema(word).otherwise:
                  throw CodlValidationError(word, InvalidKey(word))

                if fschema.unique && peers.exists(_.data.mmap(_.key) == word)
                then throw CodlValidationError(word, DuplicateKey(word))
                
                go(focus = Proto(PData(word, Nil, Layout(0, false), schema = fschema), meta2), lines = 0)
              
              case Proto(PData(key, children, layout, field@Field(arity, _)), meta) =>
                val data2 = PData(word, Nil, Layout.empty, Schema.Free)
                val layout2 = layout.copy(params = layout.params + 1)
                val focus2 = Proto(PData(key, Proto(data2).close :: children, layout2, field), meta2)
                
                go(focus = focus2, lines = 0)
                  
              case Proto(PData(key, children, layout, Schema.Free), meta) =>
                val data2 = PData(word, Nil, Layout.empty, Schema.Free)
                val layout2 = layout.copy(params = layout.params + 1)
                val focus2 = Proto(PData(key, Proto(data2).close :: children, layout2, Schema.Free), meta2)
                    
                go(focus = focus2, lines = 0)
              
              case Proto(PData(key, children, layout, schema@Struct(_, _)), meta) =>
                schema.param(children.length) match
                  case entry: Schema.Entry =>
                    val data2 = PData(word, Nil, Layout.empty, entry.schema)
                    val layout2 = layout.copy(params = layout.params + 1)
                    val focus2 = Proto(PData(key, Proto(data2).close :: children, layout2, schema), meta2)
                    
                    go(focus = focus2, lines = 0)
                  case Unset =>
                    throw CodlValidationError(word, SurplusParams(key))
          
          case Token.Comment(txt, _, _) => focus match
            case Proto(Unset, meta) =>
              val meta2 = meta.otherwise(Meta()).copy(blank = lines)
              
              go(focus = Proto(Unset, meta2.copy(comments = txt :: meta2.comments)))
            
            case Proto(data: PData, meta) =>
              go(focus = Proto(data, meta.otherwise(Meta()).copy(remark = txt, blank = lines)))


        case empty => stack match
          case Nil =>
            focus.schema.mfold(Nil)(_.requiredKeys).foreach: key =>
              if !focus.has(key) then throw CodlValidationError(focus.key, MissingKey(key))
            
            Doc((focus.close :: peers).reverse, baseSchema, margin)
          case _   => go(LazyList(Token.Outdent(stack.length + 1)))
    
    if stream.isEmpty then Doc()
    else recur(stream, Proto(), Nil, Nil, 0)

  def tokenize(in: Reader)(using Log): (Int, LazyList[Token]) throws CodlParseError =
    val reader: PositionReader = PositionReader(in)

    enum State:
      case Word, Hash, Comment, Indent, Space, Tab, Margin
    
    import State.*

    @tailrec
    def cue(): Character =
      val ch = reader.next()
      if ch.char == '\n' || ch.char == ' ' then cue() else ch
    
    val first: Character = cue()
    val margin: Int = first.column

    def istream(char: Character, state: State = Indent, indent: Int = margin): LazyList[Token] =
      stream(char, state, indent)
    
    @tailrec
    def stream(char: Character, state: State = Indent, indent: Int = margin): LazyList[Token] =
      inline def next(): Character = reader.next()
      inline def recur(state: State, indent: Int = indent): LazyList[Token] = stream(next(), state, indent)
      inline def irecur(state: State, indent: Int = indent): LazyList[Token] = istream(next(), state, indent)
      inline def diff: Int = char.column - indent

      def token(): Token = state match
        case Comment => Token.Comment(reader.get(), reader.start()(0), reader.start()(1))
        case Margin  => val text = reader.get()
                        val trimmed = if text.last == '\n' then text.drop(1, Rtl) else text
                        Token.Item(trimmed, reader.start()(0), reader.start()(1), true)
        case Word    => Token.Item(reader.get(), reader.start()(0), reader.start()(1), false)
        case _       => ???

      def put(next: State, stop: Boolean = false): LazyList[Token] = token() #:: irecur(next)
      
      inline def consume(next: State): LazyList[Token] =
        reader.put(char)
        recur(next)

      inline def block(): LazyList[Token] =
        if diff >= 4 then consume(Margin)
        else if char.char == ' ' then recur(Margin)
        else token() #:: istream(char)
        
      inline def newline(next: State): LazyList[Token] =
        if diff > 4 then throw CodlParseError(char.line, char.column, SurplusIndent)
        else if diff%2 != 0 then throw CodlParseError(char.line, char.column, UnevenIndent(margin, char.column))
        else diff match
          case 2 => Token.Indent #:: irecur(next, indent = char.column)
          case 0 => Token.Peer #:: irecur(next, indent = char.column)
          case n => Token.Outdent(-diff/2) #:: irecur(next, indent = char.column)

      char.char match
        case _ if char == Character.End => state match
          case Indent | Space | Tab | Hash => LazyList()
          case Comment | Word | Margin     => LazyList(token())
        
        case '\n' => state match
          case Word | Comment       => put(Indent)
          case Margin               => block()
          case Indent | Space | Tab => Token.Blank #:: irecur(Indent)
          case _                    => recur(Indent)
        
        case ' ' => state match
          case Space | Tab => recur(Tab)
          case Indent      => recur(Indent)
          case Word        => put(Space)
          case Comment     => consume(Comment)
          case Margin      => block()
          case Hash        => reader.get(); recur(Comment)
        
        case '#' => state match
          case Space | Tab    => consume(Hash)
          case Word | Comment => consume(state)
          case Indent         => if diff == 4 then recur(Margin) else newline(Comment)
          case Margin         => block()
          case Hash           => consume(Word)
        
        case ch => state match
          case Space | Tab | Word => consume(Word)
          case Comment            => consume(Comment)
          case Indent             => reader.put(char); if diff == 4 then recur(Margin) else newline(Word)
          case Margin             => block()
          
          case Hash => char.char match
            case '!' if first.line == 0 && first.column == 1 => consume(Comment)
            case ch                                          => consume(Word)

    (first.column, stream(first).drop(1))

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

// object Multiplicity:
//   def parse(symbol: Char): Multiplicity = symbol match
//     case '+' => Multiplicity.AtLeastOne
//     case '?' => Multiplicity.Optional
//     case '*' => Multiplicity.Many
//     case '&' => Multiplicity.Joined
//     case '!' => Multiplicity.Unique
//     case _   => Multiplicity.One
