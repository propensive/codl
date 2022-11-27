package cellulose

import rudiments.*
import eucalyptus.*

import java.io as ji

import language.experimental.captureChecking
import language.dynamics

object Schema:
  case class Entry(key: Maybe[Text], schema: Schema):
    export schema.arity.{required, variadic, unique}

  object Free extends Struct(List(Entry(Unset, Field(Arity.Many))), Arity.Many):
    override def apply(key: Text): Maybe[Schema] = Free
    override def optional = Free
    override def toString(): String = "%"

  def apply(subschemas: List[(Text, Schema)]): Schema = Struct(subschemas.map(Entry(_, _)), Arity.Optional)

sealed trait Schema(protected val subschemas: IArray[Schema.Entry], val arity: Arity,
                        val validator: Maybe[Text -> Boolean])
extends Dynamic:
  import Schema.{Free, Entry}
  protected lazy val dictionary: Map[Maybe[Text], Schema] = subschemas.map(_.tuple).to(Map)
  
  lazy val keyMap: Map[Maybe[Text], Int] = subschemas.map(_.key).zipWithIndex.to(Map)

  def optional: Schema
  def entry(n: Int): Entry = subschemas(n)
  def parse(text: Text)(using Log): Doc throws CodlParseError | CodlValidationError =
    Codl.parse(ji.StringReader(text.s), this)
  
  def apply(key: Text): Maybe[Schema] = dictionary.get(key).orElse(dictionary.get(Unset)).getOrElse(Unset)
  def has(key: Maybe[Text]): Boolean = dictionary.contains(key)
  lazy val requiredKeys: List[Text] = subschemas.filter(_.required).map(_.key).sift[Text].to(List)

  export arity.{required, variadic, unique}

enum Arity:
  case One, AtLeastOne, Optional, Many, Unique

  def required: Boolean = this == One || this == Unique || this == AtLeastOne
  def variadic: Boolean = this == AtLeastOne || this == Many
  def unique: Boolean = !variadic

object Struct:
  def apply(subschemas: (Text, Schema)*): Struct =
    Struct(subschemas.map(Schema.Entry(_, _)).to(List), Arity.Optional)

case class Struct(structSubschemas: List[Schema.Entry], structArity: Arity = Arity.Optional)
extends Schema(IArray.from(structSubschemas), structArity, Unset):
  import Schema.{Free, Entry}
  
  def optional: Struct = Struct(structSubschemas, Arity.Optional)
  def param(n: Int): Maybe[Entry] = params.lift(n).getOrElse(if params.last.variadic then params.last else Unset)
  
  lazy val params: IArray[Entry] =
    def recur(subschemas: List[Entry], fields: List[Entry]): IArray[Entry] = subschemas match
      case Nil                                             => IArray.from(fields.reverse)
      case Entry(key, struct: Struct) :: _                 => recur(Nil, fields)
      case Entry(key, field: Field) :: _ if field.variadic => recur(Nil, Entry(key, field) :: fields)
      case Entry(key, field: Field) :: tail                => recur(tail, Entry(key, field) :: fields)

    recur(subschemas.to(List), Nil)

case class Field(fieldArity: Arity, fieldValidator: Maybe[Text -> Boolean] = Unset)
extends Schema(IArray(), fieldArity, fieldValidator):
  def optional: Field = Field(Arity.Optional, fieldValidator)