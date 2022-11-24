package cellulose

import rudiments.*
import gossamer.*
import eucalyptus.*

import java.io as ji

import language.dynamics

object Node:
  given DebugString[Node] = _.data.option.fold(t"!"): data =>
    t"${data.key}[${data.children.map(_.debug).join(t",")}]"

  val empty: Node = Node()
  def apply(key: Text)(child: Node*): Node = Node(Data(key, IArray.from(child)))

case class Node(data: Maybe[Data] = Unset, meta: Maybe[Meta] = Unset) extends Dynamic:
  def key: Maybe[Text] = data.mmap(_.key)
  def empty: Boolean = unsafely(data.unset || data.assume.children.isEmpty)
  def schema: Maybe[Schema] = data.mmap(_.schema)
  def layout: Maybe[Layout] = data.mmap(_.layout)

  def selectDynamic(key: String): List[Data] throws MissingValueError =
    data.option.getOrElse(throw MissingValueError(key.show)).selectDynamic(key)
  
  def applyDynamic(key: String)(idx: Int = 0): Data throws MissingValueError = selectDynamic(key)(idx)

  def has(key: Maybe[Text]): Boolean = data.mmap(_.has(key)).otherwise(false)
  
  def untyped: Node =
    val data2 = data.mmap { data => Data(data.key, children = data.children.map(_.untyped)) }
    Node(data2, meta)
  
  def uncommented: Node =
    val data2 = data.mmap { data => Data(data.key, children = data.children.map(_.uncommented), Layout.empty, data.schema) }
    Node(data2, Unset)

  def wiped: Node = untyped.uncommented

object Doc:
  def apply(nodes: Node*): Doc = Doc(IArray.from(nodes), Schema.Free, 0)

case class Doc(children: IArray[Node], schema: Schema, margin: Int) extends Indexed:
  override def toString(): String = s"[[${children.mkString(" ")}]]"
  
  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: Doc =>
      schema == that.schema && margin == that.margin && children.size == that.children.size &&
          children.indices.forall { i => children(i) == that.children(i) }
    case _ => false

  override def hashCode: Int =
    children.foldLeft(margin.hashCode ^ schema.hashCode ^ children.size.hashCode)(_ ^ _.hashCode)


  def as[T: Codec]: T = summon[Codec[T]].deserialize(children)
  def paramCount: Int = 0
  def uncommented: Doc = Doc(children.map(_.uncommented), schema, margin)
  def untyped: Doc = Doc(children.map(_.untyped), Schema.Free, margin)
  def wiped = uncommented.untyped

  def binary(using Log): Text =
    val writer: ji.Writer = ji.StringWriter()
    Bin.write(writer, this)
    writer.toString().show
    

case class Data(key: Text, children: IArray[Node] = IArray(), layout: Layout = Layout.empty, schema: Schema = Schema.Free)
extends Indexed:
  def paramCount: Int = layout.mfold(0)(_.params)
  
  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: Data =>
      key == that.key && layout == that.layout && schema == that.schema && children.size ==
          that.children.size && children.indices.forall { i => children(i) == that.children(i) }
    case _ => false

  override def hashCode: Int =
    children.foldLeft(key.hashCode ^ layout.hashCode ^ schema.hashCode ^ children.size.hashCode)(_ ^ _.hashCode)

case class Meta(blank: Int = 0, comments: List[Text] = Nil, remark: Maybe[Text] = Unset, tabs: Tabs = Tabs())
object Layout:
  final val empty = Layout(0, false)

case class Layout(params: Int, multiline: Boolean)
case class Tabs(stops: TreeSet[Int] = TreeSet())

trait Indexed extends Dynamic:
  def children: IArray[Node]
  def schema: Schema
  def paramCount: Int

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

  def has(key: Maybe[Text]): Boolean = dictionary.contains(key)
  
  def apply(idx: Int = 0): Data throws MissingIndexValueError =
    if idx < array.length then array(idx) else throw MissingIndexValueError(idx)

  def selectDynamic(key: String): List[Data] throws MissingValueError =
    dictionary.get(key.show).getOrElse(throw MissingValueError(key.show))
  
  def applyDynamic(key: String)(idx: Int = 0): Data throws MissingValueError = selectDynamic(key)(idx)