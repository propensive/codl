package cellulose

import rudiments.*
import gossamer.*
import eucalyptus.*
import quagmire.*

import java.io as ji

import language.experimental.captureChecking
import language.dynamics

object Node:
  given DebugString[Node] = _.data.option.fold(t"!"): data =>
    t"${data.key}[${data.children.map(_.debug).join(t",")}]"

  val empty: Node = Node()
  def apply(key: Text)(child: Node*): Node = Node(Data(key, IArray.from(child), index = Map()))

case class Node(data: Maybe[Data] = Unset, meta: Maybe[Meta] = Unset) extends Dynamic:
  def key: Maybe[Text] = data.mm(_.key)
  def empty: Boolean = unsafely(data.unset || data.assume.children.isEmpty)
  def schema: Maybe[Schema] = data.mm(_.schema)
  def layout: Maybe[Layout] = data.mm(_.layout)
  def id: Maybe[Text] = data.mm(_.id)

  def selectDynamic(key: String): List[Data] throws MissingValueError =
    data.option.getOrElse(throw MissingValueError(key.show)).selectDynamic(key)
  
  def applyDynamic(key: String)(idx: Int = 0): Data throws MissingValueError = selectDynamic(key)(idx)

  def has(key: Maybe[Text]): Boolean = data.mm(_.has(key)).or(false)
  
  def untyped: Node =
    val data2 = data.mm { data => Data(data.key, children = data.children.map(_.untyped), index = data.index) }
    Node(data2, meta)
  
  def uncommented: Node =
    val data2 = data.mm { data => Data(data.key, children = data.children.map(_.uncommented), Layout.empty, data.schema, data.index) }
    Node(data2, Unset)

  def wiped: Node = untyped.uncommented

object Doc:
  def apply(nodes: Node*): Doc = Doc(IArray.from(nodes), Schema.Free, 0)
  
  def apply(children: IArray[Node], schema: Schema, margin: Int): Doc =
    val index = children.zipWithIndex.foldLeft(Map[Text, Int]()):
      case (map, (child, idx)) => child.mm(_.id).fm(map)(map.updated(_, idx))
    
    Doc(children, schema, margin, index)


case class Doc(children: IArray[Node], schema: Schema, margin: Int, index: Map[Text, Int]) extends Indexed:
  override def toString(): String = s"[[${children.mkString(" ")}]]"
  
  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: Doc => schema == that.schema && margin == that.margin && children.sameElements(that.children)
    case _         => false

  override def hashCode: Int = children.toSeq.hashCode ^ schema.hashCode ^ margin.hashCode


  def merge(input: Doc): Doc =
    def cmp(x: Node, y: Node): Boolean = if x.unset || y.unset then x == y else x.id == y.id

    def recur(original: IArray[Node], updates: IArray[Node]): IArray[Node] =
      val diff = Diff.diff[Node](children, updates, cmp)
      
      val nodes2 = diff.changes.foldLeft(List[Node]()):
        case (nodes, Change.Del(left, value))         => nodes
        case (nodes, Change.Ins(right, value))        => value :: nodes
        case (nodes, Change.Keep(left, right, value)) =>
          val orig: Node = original(left)
          val origData: Data = orig.data.or(???)
          
          if orig.id.unset || updates(right).id.unset then orig :: nodes
          else
            val children2 = recur(origData.children, updates(right).data.or(???).children)
            // FIXME: Check layout remains safe
            orig.copy(data = origData.copy(children = children2)) :: nodes
      
      IArray.from(nodes2.reverse)
    
    copy(children = recur(children, input.children))


  def as[T: Codec]: T = summon[Codec[T]].deserialize(children)
  def paramCount: Int = 0
  def uncommented: Doc = Doc(children.map(_.uncommented), schema, margin)
  def untyped: Doc = Doc(children.map(_.untyped), Schema.Free, margin)
  def wiped = uncommented.untyped

  def binary(using Log): Text =
    val writer: ji.Writer = ji.StringWriter()
    Bin.write(writer, this)
    writer.toString().show

  def serialize: Text =
    val writer: ji.Writer = ji.StringWriter()
    Printer.print(writer, this)
    writer.toString().show


case class Data(key: Text, children: IArray[Node] = IArray(), layout: Layout = Layout.empty, schema: Schema = Schema.Free, index: Map[Text, Int] = Map())
extends Indexed:

  def id: Maybe[Text] = schema.subschemas.find(_.schema.arity == Arity.Unique) match
    case Some(Schema.Entry(name: Text, schema)) =>
      safely(selectDynamic(name.s)).mm(_.headOption.maybe).mm(_.key)
    case _ => key
  
  def paramCount: Int = layout.fm(0)(_.params)
  
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
  def index: Map[Text, Int]

  private lazy val array: IArray[Data] = IArray.from(children.map(_.data).sift[Data])
  
  private lazy val dictionary: Map[Maybe[Text], List[Data]] =
    val data = children.map(_.data).sift[Data]
    
    val init: Map[Maybe[Text], List[Data]] = schema match
      case schema@Struct(_, _) =>
        array.take(paramCount).zipWithIndex.foldLeft(Map[Maybe[Text], List[Data]]()):
          case (acc, (param, idx)) => acc.plus(schema.param(idx).mm(_.key), param)
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