package codl

import rudiments.*
import eucalyptus.*

import java.io as ji

import language.dynamics

object Schema:
  case class Entry(key: Maybe[Text], schema: Schema):
    export schema.arity.{required, variadic, unique}

  object Freeform extends Struct(List(Entry(Unset, Field(Arity.Many))), Arity.Many):
    override def apply(key: Text): Maybe[Schema] = Freeform

    override def toString(): String = "%"

  def apply(subschemas: List[(Text, Schema)]): Schema = Struct(subschemas.map(Entry(_, _)), Arity.Optional)

sealed trait Schema(protected val subschemas: List[Schema.Entry] = Nil, val arity: Arity = Arity.Optional,
                        val validator: Maybe[Text => Boolean] = Unset)
extends Dynamic:
  import Schema.{Freeform, Entry}
  protected lazy val dictionary: Map[Maybe[Text], Schema] = subschemas.map(_.tuple).to(Map)
  
  def parse(text: Text)(using Log): Doc throws CodlParseError | CodlValidationError =
    Codl.parse(ji.StringReader(text.s), this)
  
  def apply(key: Text): Maybe[Schema] = dictionary.get(key).orElse(dictionary.get(Unset)).getOrElse(Unset)
  def has(key: Maybe[Text]): Boolean = dictionary.contains(key)
  def requiredKeys: List[Text] = subschemas.filter(_.required).map(_.key).sift[Text]

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
extends Schema(structSubschemas, structArity, Unset):
  import Schema.{Freeform, Entry}
  
  def param(n: Int): Maybe[Entry] = params.lift(n).getOrElse(if params.last.variadic then params.last else Unset)
  
  lazy val params: IArray[Entry] =
    def recur(subschemas: List[Entry], fields: List[Entry]): IArray[Entry] = subschemas match
      case Nil                                             => IArray.from(fields.reverse)
      case Entry(key, struct: Struct) :: _                 => recur(Nil, fields)
      case Entry(key, field: Field) :: _ if field.variadic => recur(Nil, Entry(key, field) :: fields)
      case Entry(key, field: Field) :: tail                => recur(tail, Entry(key, field) :: fields)

    recur(subschemas, Nil)

case class Field(fieldArity: Arity, fieldValidator: Maybe[Text => Boolean] = Unset)
extends Schema(Nil, fieldArity, fieldValidator)