package cellulose

import rudiments.*
import gossamer.*
import wisteria.*

import Arity.*

trait Codec[T]:
  def serialize(value: T): List[Node]
  def deserialize(value: List[Node]): T
  def schema: Schema

  protected def readField(nodes: List[Node]): Maybe[Text] = nodes match
    case List(Node(Data(value, _, _, _), _)) => value
    case _                                   => Unset

object Codec extends Derivation[Codec]:
  
  def join[T](ctx: CaseClass[Codec, T]): Codec[T] = new Codec[T]:
    def schema: Schema =
      val entries: IArray[Schema.Entry] = ctx.params.map: param =>
        Schema.Entry(param.label.show, param.typeclass.schema)

      Struct(entries.to(List), Arity.One)
    
    def serialize(value: T): List[Node] =
      ctx.params.map: p =>
        Node(Data(p.label.show, p.typeclass.serialize(p.deref(value)).to(List), Layout.empty, p.typeclass.schema))
      .to(List).filter(!_.empty)

    def deserialize(value: List[Node]): T =
      val dict = value.map { case Node(Data(key, children, _, _), _) => key -> children }.to(Map)
      ctx.construct { param => param.typeclass.deserialize(dict.get(param.label.show).getOrElse(Nil)) }
  
  def split[T](ctx: SealedTrait[Codec, T]): Codec[T] = ???
  
  given Codec[Byte] with
    def schema = Field(Arity.One)
    def serialize(value: Byte): List[Node] = List(Node(Data(value.show)))
    def deserialize(value: List[Node]): Byte = readField(value).option.get.s.toInt.toByte
  
  given Codec[Short] with
    def schema = Field(Arity.One)
    def serialize(value: Short): List[Node] = List(Node(Data(value.show)))
    def deserialize(value: List[Node]): Short = readField(value).option.get.s.toShort

  given Codec[Long] with
    def schema = Field(Arity.One)
    def serialize(value: Long): List[Node] = List(Node(Data(value.show)))
    def deserialize(value: List[Node]): Long = readField(value).option.get.s.toLong

  given Codec[Char] with
    def schema = Field(Arity.One)
    def serialize(value: Char): List[Node] = List(Node(Data(value.show)))
    def deserialize(value: List[Node]): Char = unsafely(readField(value).option.get(0))

  given Codec[Int] with
    def schema = Field(Arity.One)
    def serialize(value: Int): List[Node] = List(Node(Data(value.show)))
    def deserialize(value: List[Node]): Int = readField(value).option.get.s.toInt

  given Codec[Text] with
    def schema = Field(Arity.One)
    def serialize(value: Text): List[Node] = List(Node(Data(value.show)))
    def deserialize(value: List[Node]): Text = readField(value).option.get

  given Codec[Boolean] with
    def schema = Field(Arity.One)
    def serialize(value: Boolean): List[Node] = List(Node(Data(if value then t"yes" else t"no")))

    def deserialize(value: List[Node]): Boolean = value match
      case List(Node(Data(t"yes", _, _, _), _)) => true
      case List(Node(Data(t"no", _, _, _), _))  => false
  
  given maybe[T](using codec: Codec[T]): Codec[Maybe[T]] = new Codec[Maybe[T]]:
    def schema: Schema = summon[Codec[T]].schema.optional
  
    def serialize(value: Maybe[T]): List[Node] = value match
      case Unset               => List()
      case value: T @unchecked => codec.serialize(value)
    
    def deserialize(value: List[Node]): Maybe[T] =
      if value.isEmpty then Unset else codec.deserialize(value)
    
  given list[T](using codec: Codec[T]): Codec[List[T]] = new Codec[List[T]]:
    def schema: Schema = codec.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => Struct(List(Schema.Entry(t"item", struct)), Arity.Many)

    def serialize(value: List[T]): List[Node] = codec.schema match
      case Field(_, _)  => value.flatMap(codec.serialize)
      case Struct(_, _) => value.map { item => Node(Data(t"item", codec.serialize(item))) }

    def deserialize(value: List[Node]): List[T] = schema match
      case Field(_, _) => value.map:
        case node@Node(Data(key, _, _, _), _) => codec.deserialize(List(node))
      case Struct(_, _) => value.map:
        case Node(Data(_, children, _, _), _) => codec.deserialize(children)
  
extension [T](value: T)(using codec: Codec[T])
  def codl: Doc = Doc(codec.serialize(value), codec.schema, 0)