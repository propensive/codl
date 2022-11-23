package codl

import rudiments.*
import gossamer.*

import language.dynamics

object Node:
  given DebugString[Node] = _.data.option.fold(t"!"): data =>
    t"${data.key}[${data.children.map(_.debug).join(t",")}]"

  val empty: Node = Node()
  def apply(key: Text)(child: Node*): Node = Node(Data(key, child.to(List), Unset))

case class Node(data: Maybe[Data] = Unset, meta: Maybe[Meta] = Unset) extends Dynamic:
  def key: Maybe[Text] = data.mmap(_.key)
  def empty: Boolean = data.unset && meta.unset
  def schema: Maybe[Schema] = data.mmap(_.schema)
  def layout: Maybe[Layout] = data.mmap(_.layout)

  def selectDynamic(key: String): List[Data] throws MissingValueError =
    data.option.getOrElse(throw MissingValueError(key.show)).selectDynamic(key)
  
  def applyDynamic(key: String)(idx: Int = 0): Data throws MissingValueError = selectDynamic(key)(idx)

  def has(key: Maybe[Text]): Boolean = data.mmap(_.has(key)).otherwise(false)
  
  def close: Node = data.mfold(Node(Unset, meta)):
    case Data(key, children, layout, schema) =>
      Node(Data(key, children.reverse, layout, schema), meta.mmap { m => m.copy(comments = m.comments.reverse) })

  def untyped: Node =
    val data2 = data.mmap { data => Data(data.key, children = data.children.map(_.untyped), Unset) }
    Node(data2, meta)
  
  def uncommented: Node =
    val data2 = data.mmap { data => Data(data.key, children = data.children.map(_.uncommented), Unset, data.schema) }
    Node(data2, Unset)

  def wiped: Node = untyped.uncommented

object Doc:
  def apply(node: Node*): Doc = Doc(node.to(List), Schema.Freeform, 0)

case class Doc(children: List[Node], schema: Schema, margin: Int) extends Indexed:
  override def toString(): String = s"[[${children.mkString(" ")}]]"
  def paramCount: Int = 0
  def uncommented: Doc = Doc(children.map(_.uncommented), schema, margin)
  def untyped: Doc = Doc(children.map(_.untyped), schema, margin)
  def wiped = uncommented.untyped

case class Data(key: Text, children: List[Node] = Nil, layout: Maybe[Layout] = Unset, schema: Schema = Schema.Freeform)
extends Indexed:
  def paramCount: Int = layout.mfold(0)(_.params)

case class Meta(blank: Int = 0, comments: List[Text] = Nil, remark: Maybe[Text] = Unset, tabs: Tabs = Tabs())
case class Layout(params: Int, multiline: Boolean)
case class Tabs(stops: TreeSet[Int] = TreeSet())

trait Indexed extends Dynamic:
  def children: List[Node]
  def schema: Schema
  def paramCount: Int

  private lazy val index: IArray[Data] = IArray.from(children.map(_.data).sift[Data])
  
  private lazy val dictionary: Map[Maybe[Text], List[Data]] =
    val data = children.map(_.data).sift[Data]
    
    val init: Map[Maybe[Text], List[Data]] = schema match
      case schema@Struct(_, _) =>
        index.take(paramCount).zipWithIndex.foldLeft(Map[Maybe[Text], List[Data]]()):
          case (acc, (param, idx)) => acc.plus(schema.param(idx).mmap(_.key), param)
      case Field(_, _) =>
        Map()
    
    data.drop(paramCount).foldLeft(init): (acc, data) =>
      acc.plus(data.key, data)
    .view.mapValues(_.reverse).to(Map)

  def has(key: Maybe[Text]): Boolean = dictionary.contains(key)
  
  def apply(idx: Int = 0): Data throws MissingIndexValueError =
    if idx < index.length then index(idx) else throw MissingIndexValueError(idx)

  def selectDynamic(key: String): List[Data] throws MissingValueError =
    dictionary.get(key.show).getOrElse(throw MissingValueError(key.show))
  
  def applyDynamic(key: String)(idx: Int = 0): Data throws MissingValueError = selectDynamic(key)(idx)