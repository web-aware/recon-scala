package net.coeffect.recon

import basis._
import basis.collections._
import basis.collections.immutable._
import basis.text._
import basis.util._
import scala.reflect._
import scala.runtime._

trait Recon { Recon =>
  type Item <: ReconItem
  type Field <: ReconField with Item
  type Attr <: ReconAttr with Field
  type Slot <: ReconSlot with Field
  type Value <: ReconValue with Item
  type Record <: ReconRecord with Value
  type Text <: ReconText with Value
  type Number <: ReconNumber with Value
  type Bool <: ReconBool with Value
  type Extant <: ReconExtant with Value
  type Absent <: ReconAbsent with Value

  val Item: ReconItemFactory
  val Field: ReconFieldFactory
  val Attr: ReconAttrFactory
  val Slot: ReconSlotFactory
  val Value: ReconValueFactory
  val Record: ReconRecordFactory
  val Text: ReconTextFactory
  val Number: ReconNumberFactory
  val Bool: ReconBoolFactory

  def True: Bool
  def False: Bool
  def Extant: Extant
  def Absent: Absent

  implicit def RecordBuilder: Builder[Item] with From[Record] with State[Record] = Record.Builder
  implicit def TextBuilder: StringBuilder with From[Text] with State[Text] = Text.Builder

  implicit lazy val StringToText: String => Text = new StringToText()
  implicit lazy val IntToNumber: Int => Number = new IntToNumber()
  implicit lazy val LongToNumber: Long => Number = new LongToNumber()
  implicit lazy val FloatToNumber: Float => Number = new FloatToNumber()
  implicit lazy val DoubleToNumber: Double => Number = new DoubleToNumber()
  implicit lazy val BooleanToBool: Boolean => Bool = new BooleanToBool()

  implicit def ItemTag: ClassTag[Item]
  implicit def FieldTag: ClassTag[Field]
  implicit def AttrTag: ClassTag[Attr]
  implicit def SlotTag: ClassTag[Slot]
  implicit def ValueTag: ClassTag[Value]
  implicit def RecordTag: ClassTag[Record]
  implicit def TextTag: ClassTag[Text]
  implicit def NumberTag: ClassTag[Number]
  implicit def BoolTag: ClassTag[Bool]
  implicit def ExtantTag: ClassTag[Extant]
  implicit def AbsentTag: ClassTag[Absent]

  implicit def ReconStringContext(stringContext: StringContext): ReconStringContext[this.type] =
    macro ReconMacros.prefixReconStringContext[this.type]

  lazy val ReconParser: ReconParser = new ReconParser()


  trait ReconItem { this: Item =>
    def isDefined: Boolean = true

    def isField: Boolean = false
    def asField: Field = throw new MatchError("not a Field")

    def isAttr: Boolean = false
    def asAttr: Attr = throw new MatchError("not an Attr")

    def isSlot: Boolean = false
    def asSlot: Slot = throw new MatchError("not a Slot")

    def isValue: Boolean = false
    def asValue: Value = throw new MatchError("not a Value")

    def isRecord: Boolean = false
    def asRecord: Record = throw new MatchError("not a Record")

    def isText: Boolean = false
    def asText: Text = throw new MatchError("not Text")

    def isNumber: Boolean = false
    def asNumber: Number = throw new MatchError("not a Number")

    def isBool: Boolean = false
    def asBool: Bool = throw new MatchError("not a Bool")

    def isExtant: Boolean = false
    def asExtant: Extant = throw new MatchError("not Extant")

    def isAbsent: Boolean = false
    def asAbsent: Absent = throw new MatchError("not Absent")

    def name: String
    def value: Value

    def / (name: String): Value

    def writeReconBlock(builder: StringBuilder): Unit = writeRecon(builder)

    private[recon] def writeRecon(builder: StringBuilder, inMarkup: Boolean): Unit = writeRecon(builder)

    def writeRecon(builder: StringBuilder): Unit

    def toRecon: String = {
      val builder = String.Builder
      writeRecon(builder)
      builder.state
    }

    def toReconBlock: String = toRecon
  }

  abstract class ReconItemFactory {
    override def toString: String = (String.Builder~Recon.toString~'.'~"Item").state
  }


  trait ReconField extends Equals with ReconItem { this: Field =>
    override def isField: Boolean = true
    override def asField: Field = this

    override def / (name: String): Value = value / name
  }

  abstract class ReconFieldFactory {
    def unapply(field: Field): Maybe[(String, Value)] = Bind((field.name, field.value))

    override def toString: String = (String.Builder~Recon.toString~'.'~"Field").state
  }


  trait ReconAttr extends Equals with ReconField { this: Attr =>
    override def isAttr: Boolean = true
    override def asAttr: Attr = this

    override def writeRecon(builder: StringBuilder): Unit = {
      builder.append('@')
      builder.append(name)
      if (!value.isExtant) {
        builder.append('(')
        value.writeReconBlock(builder)
        builder.append(')')
      }
    }

    override def canEqual(other: Any): Boolean = other.isInstanceOf[ReconAttr]

    override def equals(other: Any): Boolean = other match {
      case that: ReconAttr =>
        that.canEqual(this) && name.equals(that.name) && value.equals(that.value)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Attr], name.hashCode), value.hashCode))
    }

    override def toString: String = {
      val s = String.Builder~"Attr"~'('~>name
      if (!value.isExtant) s~", "~>value
      (s~')').state
    }
  }

  abstract class ReconAttrFactory {
    def apply(name: String, value: Value): Attr
    def apply(name: String): Attr = apply(name, Extant)

    def unapply(attr: Attr): Maybe[(String, Value)] = Bind((attr.name, attr.value))

    override def toString: String = (String.Builder~Recon.toString~'.'~"Attr").state
  }


  trait ReconSlot extends Equals with ReconField { this: Slot =>
    override def isSlot: Boolean = true
    override def asSlot: Slot = this

    override def writeRecon(builder: StringBuilder): Unit = {
      builder.append(name)
      builder.append(':')
      if (!value.isExtant) value.writeRecon(builder)
    }

    override def canEqual(other: Any): Boolean = other.isInstanceOf[ReconSlot]

    override def equals(other: Any): Boolean = other match {
      case that: ReconSlot =>
        that.canEqual(this) && name.equals(that.name) && value.equals(that.value)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Slot], name.hashCode), value.hashCode))
    }

    override def toString: String = {
      val s = String.Builder~"Slot"~'('~>name
      if (!value.isExtant) s~", "~>value
      (s~')').state
    }
  }

  abstract class ReconSlotFactory {
    def apply(name: String, value: Value): Slot
    def apply(name: String): Slot = apply(name, Extant)

    def unapply(slot: Slot): Maybe[(String, Value)] = Bind((slot.name, slot.value))

    override def toString: String = (String.Builder~Recon.toString~'.'~"Slot").state
  }


  trait ReconValue extends ReconItem { this: Value =>
    override def isValue: Boolean = true
    override def asValue: Value = this

    override def name: String = ""
    override def value: Value = this

    override def / (name: String): Value = Absent
  }

  abstract class ReconValueFactory {
    def undefined: Value = Absent

    def parseRecon(recon: String): Value = {
      val result = ReconParser.BlockParser.run(new UString(recon).iterator)
      if (result.isDone) result.bind
      else result.trap match {
        case ex: Throwable => throw ex
        case error => throw new ReconException(error.toString)
      }
    }

    def Builder: Builder[Item] with State[Value] = new ReconValueBuilder()

    override def toString: String = (String.Builder~Recon.toString~'.'~"Value").state
  }

  private final class ReconValueBuilder extends Builder[Item] with State[Value] {
    private[this] var builder: Builder[Item] with basis.State[Record] = null
    private[this] var value: Value = null.asInstanceOf[Value]

    override def append(item: Item): Unit = {
      if (builder ne null) builder.append(item)
      else if ((value eq null) && item.isValue) value = item.asValue
      else {
        builder = RecordBuilder
        if (value ne null) {
          builder.append(value)
          value = null.asInstanceOf[Value]
        }
        builder.append(item)
      }
    }

    override def clear(): Unit = {
      builder = null
      value = null.asInstanceOf[Value]
    }

    override def expect(count: Int): this.type = this

    override def state: Value = {
      if (value ne null) value
      else if (builder ne null) builder.state
      else Absent
    }

    override def toString: String = (String.Builder~Recon.toString~'.'~"Value"~'.'~"Builder").state
  }


  trait ReconRecord
    extends Equals
    with Immutable
    with Family[Record]
    with IndexedSeq[Item]
    with ReconValue { this: Record =>

    override def isRecord: Boolean = true
    override def asRecord: Record = this

    def contains(name: String): Boolean

    def apply(name: String): Value

    def :+ (item: Item): Record
    def +: (item: Item): Record
    def + (item: Item): Record
    def - (name: String): Record

    def ++ (that: Record): Record
    def -- (that: Record): Record

    def hasAttrs: Boolean = this.exists(_.isAttr)

    def isMarkup: Boolean = {
      val items = iterator
      var afterNonText = false
      var hasNonText = false
      var sections = 0
      while (!items.isEmpty) {
        val item = items.head
        if (!item.isText) {
          if (afterNonText) return false
          afterNonText = true
          hasNonText = true
        }
        else if (afterNonText && !item.asText.isBlank) {
          afterNonText = false
          sections += 1
        }
        else if (sections == 0) sections = 1
        items.step()
      }
      hasNonText && sections >= 2
    }

    override def writeReconBlock(builder: StringBuilder): Unit = {
      if (isMarkup) writeReconMarkup(builder, inMarkup = false)
      else {
        val items = iterator
        var item = null.asInstanceOf[Item]
        var hasAttrs = false
        while (!items.isEmpty && { item = items.head; item.isAttr }) {
          item.writeRecon(builder)
          items.step()
          hasAttrs = true
        }
        if (!items.isEmpty) {
          if (hasAttrs) builder.append('{')
          writeReconItem(items.head, builder)
          items.step()
          while (!items.isEmpty) {
            builder.append(',')
            writeReconItem(items.head, builder)
            items.step()
          }
          if (hasAttrs) builder.append('}')
        }
        else if (!hasAttrs) {
          builder.append('{')
          builder.append('}')
        }
      }
    }

    private[recon] override def writeRecon(builder: StringBuilder, inMarkup: Boolean): Unit = {
      if (isMarkup) writeReconMarkup(builder, inMarkup)
      else {
        val items = iterator
        var item = null.asInstanceOf[Item]
        var hasAttrs = false
        while (!items.isEmpty && { item = items.head; item.isAttr }) {
          item.writeRecon(builder)
          items.step()
          hasAttrs = true
        }
        if (!items.isEmpty) {
          items.step()
          if (hasAttrs && items.isEmpty) {
            if (inMarkup) {
              if (item.isRecord || item.isText) item.writeRecon(builder, inMarkup = true)
              else {
                builder.append('{')
                item.writeRecon(builder, inMarkup = false)
                builder.append('}')
              }
            }
            else item.writeRecon(builder, inMarkup = false)
          }
          else {
            builder.append('{')
            writeReconItem(item, builder)
            while (!items.isEmpty) {
              item = items.head
              builder.append(',')
              writeReconItem(item, builder)
              items.step()
            }
            builder.append('}')
          }
        }
        else if (!hasAttrs) {
          builder.append('{')
          builder.append('}')
        }
      }
    }

    override def writeRecon(builder: StringBuilder): Unit = writeRecon(builder, inMarkup = false)

    private def writeReconItem(item: Item, builder: StringBuilder): Unit = {
      if (item.isField) {
        builder.append(item.name)
        builder.append(':')
        if (!item.value.isExtant) item.value.writeRecon(builder)
      }
      else item.writeRecon(builder)
    }

    private def writeReconMarkup(builder: StringBuilder, inMarkup: Boolean): Unit = {
      val items = iterator
      var item = null.asInstanceOf[Item]
      var hasAttrs = false
      while (!items.isEmpty && { item = items.head; item.isAttr }) {
        item.writeRecon(builder, inMarkup = true)
        items.step()
        hasAttrs = true
      }
      builder.append('[')
      while (!items.isEmpty) {
        item = items.head
        if (item.isText) {
          val cs = item.asText.iterator
          while (!cs.isEmpty) {
            cs.head match {
              case c @ ('\\' | '@' | '{' | '}' | '[' | ']') =>
                builder.append('\\'); builder.append(c)
              case c => builder.append(c)
            }
            cs.step()
          }
        }
        else if (item.isRecord) item.writeRecon(builder, inMarkup = true)
        else {
          builder.append('{')
          item.writeRecon(builder)
          builder.append('}')
        }
        items.step()
      }
      builder.append(']')
    }

    protected override def stringPrefix: String = "Record"
  }

  abstract class ReconRecordFactory extends special.SeqSource[Record, Item] {
    override def toString: String = (String.Builder~Recon.toString~'.'~"Record").state
  }


  trait ReconText extends Equals with Family[Text] with UTF with ReconValue { this: Text =>
    override def isText: Boolean = true
    override def asText: Text = this

    def isBlank: Boolean = this.forall { c => c == 0x20 || c == 0x9 }

    private[recon] override def writeRecon(builder: StringBuilder, inMarkup: Boolean): Unit = {
      val cs = iterator
      if (inMarkup) {
        builder.append('[')
        while (!cs.isEmpty) {
          cs.head match {
            case c @ ('\\' | '@' | '{' | '}' | '[' | ']') =>
              builder.append('\\'); builder.append(c)
            case c => builder.append(c)
          }
          cs.step()
        }
        builder.append(']')
      }
      else {
        builder.append('"')
        while (!cs.isEmpty) {
          cs.head match {
            case c @ ('"' | '\\' | '@' | '{' | '}' | '[' | ']') =>
                         builder.append('\\'); builder.append(c)
            case '\b' => builder.append('\\'); builder.append('b')
            case '\f' => builder.append('\\'); builder.append('f')
            case '\n' => builder.append('\\'); builder.append('n')
            case '\r' => builder.append('\\'); builder.append('r')
            case '\t' => builder.append('\\'); builder.append('t')
            case c    => builder.append(c)
          }
          cs.step()
        }
        builder.append('"')
      }
    }

    override def writeRecon(builder: StringBuilder): Unit = writeRecon(builder, inMarkup = false)

    protected override def stringPrefix: String = "Text"
  }

  abstract class ReconTextFactory extends StringFactory[Text] {
    def unapply(text: Text): Maybe[String] = Bind(text.toUString.toString)

    override def toString: String = (String.Builder~Recon.toString~'.'~"Text").state
  }


  trait ReconNumber extends Equals with ReconValue { this: Number =>
    override def isNumber: Boolean = true
    override def asNumber: Number = this

    def isNaN: Boolean
    def isInfinite: Boolean

    def isValidByte: Boolean
    def isValidShort: Boolean
    def isValidInt: Boolean
    def isValidLong: Boolean
    def isValidFloat: Boolean
    def isValidDouble: Boolean

    def toByte: Byte
    def toShort: Short
    def toInt: Int
    def toLong: Long
    def toFloat: Float
    def toDouble: Double

    def toDecimalString: String

    override def writeRecon(builder: StringBuilder): Unit = builder.append(toDecimalString)

    override def toRecon: String = toDecimalString

    override def canEqual(other: Any): Boolean = other.isInstanceOf[ReconNumber]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
      case that: ReconNumber =>
        that.canEqual(this) &&
        isValidByte && that.isValidByte && toByte == that.toByte ||
        isValidShort && that.isValidShort && toShort == that.toShort ||
        isValidInt && that.isValidInt && toInt == that.toInt ||
        isValidLong && that.isValidLong && toLong == that.toLong ||
        isValidFloat && that.isValidFloat && toFloat == that.toFloat ||
        isValidDouble && that.isValidDouble && toDouble == that.toDouble ||
        toDecimalString.equals(that.toDecimalString)
      case _ => false
    })

    override def hashCode: Int = {
      import MurmurHash3._
      val h =
        if (isValidByte) hash(toByte)
        else if (isValidShort) hash(toShort)
        else if (isValidInt) hash(toInt)
        else if (isValidLong) hash(toLong)
        else if (isValidFloat) hash(toFloat)
        else if (isValidDouble) hash(toDouble)
        else toDecimalString.hashCode
      mash(mix(seed[Number], h))
    }

    override def toString: String = (String.Builder~"Number"~'('~toDecimalString~')').state
  }

  protected[recon] trait ReconInt extends ReconNumber { this: Number =>
    override def isNaN: Boolean = false
    override def isInfinite: Boolean = false

    override def isValidByte: Boolean = toByte == toInt
    override def isValidShort: Boolean = toShort == toInt
    override def isValidInt: Boolean = true
    override def isValidLong: Boolean = true
    override def isValidFloat: Boolean = true
    override def isValidDouble: Boolean = true

    override def toByte: Byte = toInt.toByte
    override def toShort: Short = toInt.toShort
    override def toLong: Long = toInt.toLong
    override def toFloat: Float = toInt.toFloat
    override def toDouble: Double = toInt.toDouble
    override def toDecimalString: String = java.lang.Integer.toString(toInt)
  }

  protected[recon] trait ReconLong extends ReconNumber { this: Number =>
    override def isNaN: Boolean = false
    override def isInfinite: Boolean = false

    override def isValidByte: Boolean = toByte == toLong
    override def isValidShort: Boolean = toShort == toLong
    override def isValidInt: Boolean = toInt == toLong
    override def isValidLong: Boolean = true
    override def isValidFloat: Boolean = true
    override def isValidDouble: Boolean = true

    override def toByte: Byte = toLong.toByte
    override def toShort: Short = toLong.toShort
    override def toInt: Int = toLong.toInt
    override def toFloat: Float = toLong.toFloat
    override def toDouble: Double = toLong.toDouble
    override def toDecimalString: String = java.lang.Long.toString(toLong)
  }

  protected[recon] trait ReconFloat extends ReconNumber { this: Number =>
    override def isNaN: Boolean = java.lang.Float.isNaN(toFloat)
    override def isInfinite: Boolean = java.lang.Float.isInfinite(toFloat)

    override def isValidByte: Boolean = toByte == toFloat
    override def isValidShort: Boolean = toShort == toFloat
    override def isValidInt: Boolean = toInt == toFloat
    override def isValidLong: Boolean = toLong == toFloat
    override def isValidFloat: Boolean = true
    override def isValidDouble: Boolean = true

    override def toByte: Byte = toFloat.toByte
    override def toShort: Short = toFloat.toShort
    override def toInt: Int = toFloat.toInt
    override def toLong: Long = toFloat.toLong
    override def toDouble: Double = toFloat.toDouble
    override def toDecimalString: String = java.lang.Float.toString(toFloat)
  }

  protected[recon] trait ReconDouble extends ReconNumber { this: Number =>
    override def isNaN: Boolean = java.lang.Double.isNaN(toDouble)
    override def isInfinite: Boolean = java.lang.Double.isInfinite(toDouble)

    override def isValidByte: Boolean = toByte == toDouble
    override def isValidShort: Boolean = toShort == toDouble
    override def isValidInt: Boolean = toInt == toDouble
    override def isValidLong: Boolean = toLong == toDouble
    override def isValidFloat: Boolean = toFloat == toDouble
    override def isValidDouble: Boolean = true

    override def toByte: Byte = toDouble.toByte
    override def toShort: Short = toDouble.toShort
    override def toInt: Int = toDouble.toInt
    override def toLong: Long = toDouble.toLong
    override def toFloat: Float = toDouble.toFloat
    override def toDecimalString: String = java.lang.Double.toString(toDouble)
  }

  abstract class ReconNumberFactory {
    def apply(value: Int): Number
    def apply(value: Long): Number
    def apply(value: Float): Number
    def apply(value: Double): Number

    def apply(value: String): Number =
      try apply(java.lang.Integer.parseInt(value))
      catch {
        case _: NumberFormatException =>
          try apply(java.lang.Long.parseLong(value))
          catch {
            case _: NumberFormatException =>
              apply(java.lang.Double.parseDouble(value))
          }
      }

    override def toString: String = (String.Builder~Recon.toString~'.'~"Number").state
  }


  trait ReconBool extends Equals with ReconValue { this: Bool =>
    override def isBool: Boolean = true
    override def asBool: Bool = this

    def toBoolean: Boolean

    override def writeRecon(builder: StringBuilder): Unit =
      builder.append(if (toBoolean) "#true" else "#false")

    override def toRecon: String = if (toBoolean) "#true" else "#false"

    override def canEqual(other: Any): Boolean = other.isInstanceOf[ReconBool]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
      case that: ReconBool => that.canEqual(this) && toBoolean == that.toBoolean
      case _ => false
    })

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Bool], hash(toBoolean)))
    }

    override def toString: String = if (toBoolean) "True" else "False"
  }

  abstract class ReconBoolFactory {
    def apply(value: Boolean): Bool = if (value) True else False

    def unapply(bool: Bool): Maybe[Boolean] = Bind(bool.toBoolean)

    override def toString: String = (String.Builder~Recon.toString~'.'~"Bool").state
  }


  trait ReconExtant extends ReconValue { this: Extant =>
    override def isExtant: Boolean = true
    override def asExtant: Extant = this

    override def writeRecon(builder: StringBuilder): Unit = ()

    override def toRecon: String = ""

    override def toString: String = "Extant"
  }


  trait ReconAbsent extends ReconValue { this: Absent =>
    override def isDefined: Boolean = false

    override def isAbsent: Boolean = true
    override def asAbsent: Absent = this

    override def writeRecon(builder: StringBuilder): Unit = ()

    override def toRecon: String = ""

    override def toString: String = "Absent"
  }


  private final class StringToText extends AbstractFunction1[String, Text] {
    override def apply(value: String): Text = Text(value)
    override def toString: String = (String.Builder~Recon.toString~'.'~"StringToText").state
  }

  private final class IntToNumber extends AbstractFunction1[Int, Number] {
    override def apply(value: Int): Number = Number(value)
    override def toString: String = (String.Builder~Recon.toString~'.'~"IntToNumber").state
  }

  private final class LongToNumber extends AbstractFunction1[Long, Number] {
    override def apply(value: Long): Number = Number(value)
    override def toString: String = (String.Builder~Recon.toString~'.'~"LongToNumber").state
  }

  private final class FloatToNumber extends AbstractFunction1[Float, Number] {
    override def apply(value: Float): Number = Number(value)
    override def toString: String = (String.Builder~Recon.toString~'.'~"FloatToNumber").state
  }

  private final class DoubleToNumber extends AbstractFunction1[Double, Number] {
    override def apply(value: Double): Number = Number(value)
    override def toString: String = (String.Builder~Recon.toString~'.'~"DoubleToNumber").state
  }

  private final class BooleanToBool extends AbstractFunction1[Boolean, Bool] {
    override def apply(value: Boolean): Bool = Bool(value)
    override def toString: String = (String.Builder~Recon.toString~'.'~"BooleanToBool").state
  }


  class ReconParser extends ReconFactory with net.coeffect.recon.ReconParser { ReconParser =>
    private[recon] override type Item = Recon.Item
    private[recon] override type Field = Recon.Field
    private[recon] override type Attr = Recon.Attr
    private[recon] override type Slot = Recon.Slot
    private[recon] override type Value = Recon.Value
    private[recon] override type Record = Recon.Record
    private[recon] override type Text = Recon.Text
    private[recon] override type Number = Recon.Number
    private[recon] override type Bool = Recon.Bool
    private[recon] override type Extant = Recon.Extant
    private[recon] override type Absent = Recon.Absent

    private[recon] override def Attr(name: String, value: Value): Attr = Recon.Attr(name, value)
    private[recon] override def Attr(name: String): Attr = Recon.Attr(name)

    private[recon] override def Slot(name: String, value: Value): Slot = Recon.Slot(name, value)
    private[recon] override def Slot(name: String): Slot = Recon.Slot(name)

    private[recon] override def ValueBuilder: ItemBuilder with State[Value] = new ValueBuilder()
    private[recon] override def RecordBuilder: ItemBuilder with State[Record] = new RecordBuilder()
    private[recon] override def TextBuilder: StringBuilder with State[Text] = Recon.TextBuilder

    private[recon] override def Number(value: String): Number = Recon.Number(value)

    private[recon] override def True: Bool = Recon.True
    private[recon] override def False: Bool = Recon.False

    private[recon] override def Extant: Extant = Recon.Extant
    private[recon] override def Absent: Absent = Recon.Absent

    private final class ValueBuilder extends ItemBuilder with State[Value] {
      private[this] var builder: Builder[Item] with basis.State[Record] = null
      private[this] var value: Value = null.asInstanceOf[Value]

      override def appendField(item: Field): Unit = {
        if (builder ne null) builder.append(item)
        else {
          builder = Recon.RecordBuilder
          if (value ne null) {
            builder.append(value)
            value = null.asInstanceOf[Value]
          }
          builder.append(item)
        }
      }

      override def appendValue(item: Value): Unit = {
        if (builder ne null) builder.append(item)
        else if (value eq null) value = item
        else {
          builder = Recon.RecordBuilder
          builder.append(value)
          value = null.asInstanceOf[Value]
          builder.append(item)
        }
      }

      override def state: Value = {
        if (value ne null) value
        else if (builder ne null) builder.state
        else Absent
      }

      override def toString: String = (String.Builder~ReconParser.toString~'.'~"ValueBuilder").state
    }

    private final class RecordBuilder extends ItemBuilder with State[Record] {
      private[this] val self: Builder[Item] with basis.State[Record] = Recon.RecordBuilder
      override def appendField(item: Field): Unit = self.append(item)
      override def appendValue(item: Value): Unit = self.append(item)
      override def state: Record = self.state
    }

    override def toString: String = (String.Builder~Recon.toString~'.'~"ReconParser").state
  }
}

object Recon extends Recon {
  override type Item   = net.coeffect.recon.Item
  override type Field  = net.coeffect.recon.Field
  override type Attr   = net.coeffect.recon.Attr
  override type Slot   = net.coeffect.recon.Slot
  override type Value  = net.coeffect.recon.Value
  override type Record = net.coeffect.recon.Record
  override type Text   = net.coeffect.recon.Text
  override type Number = net.coeffect.recon.Number
  override type Bool   = net.coeffect.recon.Bool
  override type Extant = net.coeffect.recon.Extant
  override type Absent = net.coeffect.recon.Absent

  override val Item   = net.coeffect.recon.Item
  override val Field  = net.coeffect.recon.Field
  override val Attr   = net.coeffect.recon.Attr
  override val Slot   = net.coeffect.recon.Slot
  override val Value  = net.coeffect.recon.Value
  override val Record = net.coeffect.recon.Record
  override val Text   = net.coeffect.recon.Text
  override val Number = net.coeffect.recon.Number
  override val Bool   = net.coeffect.recon.Bool
  override val True   = net.coeffect.recon.True
  override val False  = net.coeffect.recon.False
  override val Extant = net.coeffect.recon.Extant
  override val Absent = net.coeffect.recon.Absent

  implicit override lazy val ItemTag: ClassTag[Item] = ClassTag(Predef.classOf[Item])
  implicit override lazy val FieldTag: ClassTag[Field] = ClassTag(Predef.classOf[Field])
  implicit override lazy val AttrTag: ClassTag[Attr] = ClassTag(Predef.classOf[Attr])
  implicit override lazy val SlotTag: ClassTag[Slot] = ClassTag(Predef.classOf[Slot])
  implicit override lazy val ValueTag: ClassTag[Value] = ClassTag(Predef.classOf[Value])
  implicit override lazy val RecordTag: ClassTag[Record] = ClassTag(Predef.classOf[Record])
  implicit override lazy val TextTag: ClassTag[Text] = ClassTag(Predef.classOf[Text])
  implicit override lazy val NumberTag: ClassTag[Number] = ClassTag(Predef.classOf[Number])
  implicit override lazy val BoolTag: ClassTag[Bool] = ClassTag(Predef.classOf[Bool])
  implicit override lazy val ExtantTag: ClassTag[Extant] = ClassTag(Predef.classOf[Extant])
  implicit override lazy val AbsentTag: ClassTag[Absent] = ClassTag(Predef.classOf[Absent])

  override def toString: String = "Recon"
}


sealed abstract class Item extends Recon.ReconItem

object Item extends Recon.ReconItemFactory


sealed abstract class Field extends Item with Recon.ReconField

object Field extends Recon.ReconFieldFactory


final class Attr(
    override val name: String,
    override val value: Value)
  extends Field with Recon.ReconAttr

object Attr extends Recon.ReconAttrFactory {
  override def apply(name: String, value: Value): Attr = new Attr(name, value)
}


final class Slot(
    override val name: String,
    override val value: Value)
  extends Field with Recon.ReconSlot

object Slot extends Recon.ReconSlotFactory {
  override def apply(name: String, value: Value): Slot = new Slot(name, value)
}


sealed abstract class Value extends Item with Recon.ReconValue

object Value extends Recon.ReconValueFactory


final class Record private[recon] (protected val self: FingerTrieSeq[Item]) extends Value with Recon.ReconRecord {
  private[this] var _index: HashTrieMap[String, Value] = null
  private[this] def index: HashTrieMap[String, Value] = {
    if (_index eq null) _index = {
      var index = HashTrieMap.empty[String, Value]
      self.foreach { item =>
        if (item.isField) index += (item.name, item.value)
      }
      index
    }
    _index
  }

  override def isEmpty: Boolean = self.isEmpty

  override def length: Int = self.length

  override def contains(name: String): Boolean =
    if (length > 8) index.contains(name)
    else self.exists(_.name.equals(name))

  override def apply(index: Int): Item = self(index)

  override def apply(name: String): Value =
    if (length > 8) index(name)
    else {
      val these = self.iterator
      while (!these.isEmpty) {
        val item = these.head
        if (item.name.equals(name)) return item.name
        these.step()
      }
      throw new NoSuchElementException(name)
    }

  override def / (name: String): Value =
    if (length > 8) { if (index.contains(name)) index(name) else Absent }
    else {
      val these = self.iterator
      while (!these.isEmpty) {
        val item = these.head
        if (item.name.equals(name)) return item.value
        these.step()
      }
      Absent
    }

  override def :+ (item: Item): Record = new Record(self :+ item)

  override def +: (item: Item): Record = new Record(item +: self)

  override def + (item: Item): Record =
    if (item.isValue || _index.ne(null) && !_index.contains(item.name)) new Record(self :+ item)
    else {
      var i = length - 1
      val name = item.name
      while (i >= 0 && !self(i).name.equals(name)) i -= 1
      if (i >= 0) {
        if (self(i).value.equals(item.value)) this
        else new Record(self.update(i, item))
      }
      else new Record(self :+ item)
    }

  override def - (name: String): Record =
    if (_index.ne(null) && !_index.contains(name)) this
    else self.filter(!_.name.equals(name))(Recon.RecordBuilder)

  override def ++ (that: Record): Record = self.++(that.self)(Recon.RecordBuilder)

  override def -- (that: Record): Record = self.filter(item => !that.contains(item.name))(Recon.RecordBuilder)

  override def iterator: Iterator[Item] = self.iterator

  override def traverse(f: Item => Unit): Unit = self.traverse(f)
}

object Record extends Recon.ReconRecordFactory {
  override val empty: Record = new Record(FingerTrieSeq.empty)

  implicit override def Builder: Builder[Item] with State[Record] =
    new RecordBuilder(FingerTrieSeq.Builder)
}

private[recon] final class RecordBuilder(self: Builder[Item] with State[FingerTrieSeq[Item]])
  extends Builder[Item] with State[Record] {
  override def append(item: Item): Unit = self.append(item)
  override def clear(): Unit = self.clear()
  override def expect(count: Int): this.type = { self.expect(count); this }
  override def state: Record = new Record(self.state)
  override def toString: String = (String.Builder~Recon.toString~'.'~"Record"~'.'~"Builder").state
}


final class Text private[recon] (protected val self: UString) extends Value with Recon.ReconText {
  private[this] var utf8Size: Int = -1
  override def utf8Length: Int = {
    if (utf8Size == -1) utf8Size = super.utf8Length
    utf8Size
  }

  override def iterator: Iterator[Int] = self.iterator

  override def toUString: UString = self
}

object Text extends Recon.ReconTextFactory {
  override val empty: Text = new Text(new UString(""))

  override def apply(chars: CharSequence): Text = new Text(new UString(chars.toString))

  implicit override def Builder: StringBuilder with State[Text] = new TextBuilder(UString.Builder)
}

private[recon] final class TextBuilder(self: StringBuilder with State[UString])
  extends StringBuilder with State[Text] {
  override def append(c: Int): Unit = self.append(c)
  override def append(cs: CharSequence): Unit = self.append(cs)
  override def clear(): Unit = self.clear()
  override def expect(count: Int): this.type = { self.expect(count); this }
  override def state: Text = new Text(self.state)
  override def toString: String = (String.Builder~Recon.toString~'.'~"Text"~'.'~"Builder").state
}


sealed abstract class Number extends Value with Recon.ReconNumber

private[recon] final class IntForm(override val toInt: Int) extends Number with Recon.ReconInt

private[recon] final class LongForm(override val toLong: Long) extends Number with Recon.ReconLong

private[recon] final class FloatForm(override val toFloat: Float) extends Number with Recon.ReconFloat

private[recon] final class DoubleForm(override val toDouble: Double) extends Number with Recon.ReconDouble

object Number extends Recon.ReconNumberFactory {
  override def apply(value: Int): Number = new IntForm(value)
  override def apply(value: Long): Number = new LongForm(value)
  override def apply(value: Float): Number = new FloatForm(value)
  override def apply(value: Double): Number = new DoubleForm(value)
}


sealed abstract class Bool extends Value with Recon.ReconBool

object True extends Bool {
  override def toBoolean: Boolean = true
}

object False extends Bool {
  override def toBoolean: Boolean = false
}

object Bool extends Recon.ReconBoolFactory


sealed abstract class Extant extends Value with Recon.ReconExtant

object Extant extends Extant


sealed abstract class Absent extends Value with Recon.ReconAbsent {
  override def isDefined: Boolean = false
}

object Absent extends Absent
