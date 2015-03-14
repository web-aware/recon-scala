package recon

import basis._
import basis.collections._
import basis.collections.immutable._
import basis.data._
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
  type Data <: ReconData with Value
  type Number <: ReconNumber with Value
  type Extant <: ReconExtant with Value
  type Absent <: ReconAbsent with Value

  val Item: ReconItemFactory
  val Field: ReconFieldFactory
  val Attr: ReconAttrFactory
  val Slot: ReconSlotFactory
  val Value: ReconValueFactory
  val Record: ReconRecordFactory
  val Text: ReconTextFactory
  val Data: ReconDataFactory
  val Number: ReconNumberFactory

  def True: Value
  def False: Value
  def Extant: Extant
  def Absent: Absent

  implicit def RecordBuilder: Builder[Item] with From[Record] with State[Record] = Record.Builder
  implicit def TextBuilder: StringBuilder with From[Text] with State[Text] = Text.Builder
  implicit def DataFramer: Framer with From[Data] with State[Data] = Data.Framer 

  // Redundant `with Value` types needed to make implicit conversions work
  // through path-dependent val imports.
  implicit lazy val StringToText: String => Text with Value = new StringToText()
  implicit lazy val IntToNumber: Int => Number with Value = new IntToNumber()
  implicit lazy val LongToNumber: Long => Number with Value = new LongToNumber()
  implicit lazy val FloatToNumber: Float => Number with Value = new FloatToNumber()
  implicit lazy val DoubleToNumber: Double => Number with Value = new DoubleToNumber()
  implicit lazy val BooleanToValue: Boolean => Value = new BooleanToValue()

  implicit def ItemTag: ClassTag[Item]
  implicit def FieldTag: ClassTag[Field]
  implicit def AttrTag: ClassTag[Attr]
  implicit def SlotTag: ClassTag[Slot]
  implicit def ValueTag: ClassTag[Value]
  implicit def RecordTag: ClassTag[Record]
  implicit def TextTag: ClassTag[Text]
  implicit def DataTag: ClassTag[Data]
  implicit def NumberTag: ClassTag[Number]
  implicit def ExtantTag: ClassTag[Extant]
  implicit def AbsentTag: ClassTag[Absent]

  implicit def ReconStringContext(stringContext: StringContext): ReconStringContext[this.type] =
    macro ReconMacros.prefixReconStringContext[this.type]

  lazy val ReconParser: ReconParser = new ReconParser()

  def apply[@specialized(Mold.Specialized) T](value: T)(implicit mold: Mold[T]): Value =
    mold.form(Recon)(value)


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

    def isData: Boolean = false
    def asData: Data = throw new MatchError("not Data")

    def isNumber: Boolean = false
    def asNumber: Number = throw new MatchError("not a Number")

    def isExtant: Boolean = false
    def asExtant: Extant = throw new MatchError("not Extant")

    def isAbsent: Boolean = false
    def asAbsent: Absent = throw new MatchError("not Absent")

    def key: Value
    def value: Value
    def target: Value = value

    def / (key: Value): Value

    def cast[T](implicit mold: Mold[T]): Maybe[T]

    def coerce[@specialized(Mold.Specialized) T](implicit mold: Mold[T]): T

    def in(recon: Recon): recon.Item

    def writeReconBlock(builder: StringBuilder): Unit = writeRecon(builder)

    private[recon] def writeRecon(builder: StringBuilder, inMarkup: Boolean): Unit = writeRecon(builder)

    def writeRecon(builder: StringBuilder): Unit

    def toRecon: String = {
      val builder = String.Builder
      writeRecon(builder)
      builder.state
    }

    def toReconBlock: String = {
      val builder = String.Builder
      writeReconBlock(builder)
      builder.state
    }
  }

  abstract class ReconItemFactory {
    override def toString: String = (String.Builder~Recon.toString~'.'~"Item").state
  }


  trait ReconField extends Equals with ReconItem { this: Field =>
    override def isField: Boolean = true
    override def asField: Field = this

    override def / (key: Value): Value = value / key

    override def cast[T](implicit mold: Mold[T]): Maybe[T] =
      value.cast[T](mold)

    override def coerce[@specialized(Mold.Specialized) T](implicit mold: Mold[T]): T =
      value.coerce[T](mold)

    override def in(recon: Recon): recon.Field
  }

  abstract class ReconFieldFactory {
    def unapply(field: Field): Maybe[(Value, Value)] = Bind((field.key, field.value))

    override def toString: String = (String.Builder~Recon.toString~'.'~"Field").state
  }


  trait ReconAttr extends Equals with ReconField { this: Attr =>
    override def isAttr: Boolean = true
    override def asAttr: Attr = this

    override def key: Text

    override def writeRecon(builder: StringBuilder): Unit = {
      builder.append('@')
      key.writeRecon(builder)
      if (!value.isExtant) {
        builder.append('(')
        value.writeReconBlock(builder)
        builder.append(')')
      }
    }

    override def in(recon: Recon): recon.Attr =
      if (recon eq Recon) asInstanceOf[recon.Attr]
      else recon.Attr(key in recon, value in recon)

    override def canEqual(other: Any): Boolean = other.isInstanceOf[ReconAttr]

    override def equals(other: Any): Boolean = other match {
      case that: ReconAttr =>
        that.canEqual(this) && key.equals(that.key) && value.equals(that.value)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Attr], key.hashCode), value.hashCode))
    }

    override def toString: String = {
      val s = String.Builder~"Attr"~'('~>key
      if (!value.isExtant) s~", "~>value
      (s~')').state
    }
  }

  abstract class ReconAttrFactory {
    def apply(key: Text, value: Value): Attr
    def apply(key: Text): Attr = apply(key, Extant)

    def unapply(attr: Attr): Maybe[(Text, Value)] = Bind((attr.key, attr.value))

    override def toString: String = (String.Builder~Recon.toString~'.'~"Attr").state
  }


  trait ReconSlot extends Equals with ReconField { this: Slot =>
    override def isSlot: Boolean = true
    override def asSlot: Slot = this

    override def writeRecon(builder: StringBuilder): Unit = {
      key.writeRecon(builder)
      builder.append(':')
      if (!value.isExtant) value.writeRecon(builder)
    }

    override def in(recon: Recon): recon.Slot =
      if (recon eq Recon) asInstanceOf[recon.Slot]
      else recon.Slot(key in recon, value in recon)

    override def canEqual(other: Any): Boolean = other.isInstanceOf[ReconSlot]

    override def equals(other: Any): Boolean = other match {
      case that: ReconSlot =>
        that.canEqual(this) && key.equals(that.key) && value.equals(that.value)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Slot], key.hashCode), value.hashCode))
    }

    override def toString: String = {
      val s = String.Builder~"Slot"~'('~>key
      if (!value.isExtant) s~", "~>value
      (s~')').state
    }
  }

  abstract class ReconSlotFactory {
    def apply(key: Value, value: Value): Slot
    def apply(key: Value): Slot = apply(key, Extant)

    def unapply(slot: Slot): Maybe[(Value, Value)] = Bind((slot.key, slot.value))

    override def toString: String = (String.Builder~Recon.toString~'.'~"Slot").state
  }


  trait ReconValue extends ReconItem { this: Value =>
    override def isValue: Boolean = true
    override def asValue: Value = this

    override def key: Value = Absent
    override def value: Value = this

    override def / (key: Value): Value = Absent

    override def cast[T](implicit mold: Mold[T]): Maybe[T] =
      mold.cast(Recon)(this)

    override def coerce[@specialized(Mold.Specialized) T](implicit mold: Mold[T]): T =
      mold.cast(Recon)(this).bindOrElse(mold.unit)

    override def in(recon: Recon): recon.Value
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

    def contains(key: Value): Boolean

    def apply(key: Value): Value

    def head: Item
    def foot: Item
    def tail: Record

    def :+ (item: Item): Record
    def +: (item: Item): Record
    def + (item: Item): Record
    def - (key: Value): Record

    def ++ (that: Record): Record
    def -- (that: Record): Record

    override def target: Value = {
      val items = iterator
      while (!items.isEmpty) {
        val item = items.head
        if (item.isValue) return item.asValue
        items.step()
      }
      Absent
    }

    def hasAttrs: Boolean = !isEmpty && (head.isAttr || foot.isAttr)

    def hasPrefixAttrs: Boolean = !isEmpty && head.isAttr

    def hasPostfixAttrs: Boolean = !isEmpty && foot.isAttr

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
      else if (hasAttrs) writeRecon(builder, inMarkup = false)
      else {
        val items = iterator
        if (!items.isEmpty) {
          writeReconItem(items.head, builder)
          items.step()
          while (!items.isEmpty) {
            builder.append(',')
            writeReconItem(items.head, builder)
            items.step()
          }
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
            else if (item.isRecord || item.isSlot) {
              builder.append('{')
              item.writeRecon(builder, inMarkup = false)
              builder.append('}')
            }
            else {
              builder.append(' ');
              item.writeRecon(builder, inMarkup = false)
            }
          }
          else if (!items.isEmpty && items.dup.forall(_.isAttr)) {
            if (item.isRecord || item.isSlot) {
              builder.append('{')
              item.writeRecon(builder, inMarkup = false)
              builder.append('}')
            }
            else {
              if (hasAttrs) builder.append(' ')
              item.writeRecon(builder, inMarkup = false)
            }
          }
          else {
            builder.append('{')
            writeReconItem(item, builder)
            while (!items.isEmpty && ({ item = items.head; !item.isAttr } || !items.dup.forall(_.isAttr))) {
              builder.append(',')
              writeReconItem(item, builder)
              items.step()
            }
            builder.append('}')
          }
          while (!items.isEmpty) {
            items.head.asAttr.writeRecon(builder)
            items.step()
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
        item.key.writeRecon(builder)
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
              case c @ ('@' | '[' | '\\' | ']' | '{' | '}') =>
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

    override def in(recon: Recon): recon.Record =
      if (recon eq Recon) asInstanceOf[recon.Record]
      else this.map(_ in recon)(recon.RecordBuilder)

    def drop(lower: Int): Record

    def take(upper: Int): Record

    def valuesIterator: Iterator[Value] = new ReconRecordValuesIterator(iterator)

    protected override def stringPrefix: String = "Record"
  }

  abstract class ReconRecordFactory extends special.SeqSource[Record, Item] {
    def parseRecon(recon: String): Record = {
      val result = ReconParser.BlockParser.run(new UString(recon).iterator)
      if (result.isDone) {
        val value = result.bind
        if (value.isRecord) value.asRecord
        else apply(value)
      }
      else result.trap match {
        case ex: Throwable => throw ex
        case error => throw new ReconException(error.toString)
      }
    }

    override def toString: String = (String.Builder~Recon.toString~'.'~"Record").state
  }

  private final class ReconRecordValuesIterator(self: Iterator[Item]) extends Iterator[Value] {
    override def isEmpty: Boolean = self.isEmpty

    override def head: Value = self.head.value

    override def step(): Unit = self.step()

    override def dup: Iterator[Value] = new ReconRecordValuesIterator(self.dup)
  }


  trait ReconText extends Equals with Family[Text] with UTF with ReconValue { this: Text =>
    override def isText: Boolean = true
    override def asText: Text = this

    def isIdent: Boolean = {
      val cs = iterator
      !cs.isEmpty && ReconParser.isNameStartChar(cs.head) && {
        cs.step()
        while (!cs.isEmpty && ReconParser.isNameChar(cs.head)) cs.step()
        cs.isEmpty
      }
    }

    def isBlank: Boolean = this.forall { c => c == 0x20 || c == 0x9 }

    private[recon] def writeReconMarkup(builder: StringBuilder): Unit = {
      val cs = iterator
      builder.append('[')
      while (!cs.isEmpty) {
        cs.head match {
          case c @ ('@' | '[' | '\\' | ']' | '{' | '}') =>
            builder.append('\\'); builder.append(c)
          case c => builder.append(c)
        }
        cs.step()
      }
      builder.append(']')
    }

    private[recon] def writeReconString(builder: StringBuilder): Unit = {
      val cs = iterator
      builder.append('"')
      while (!cs.isEmpty) {
        cs.head match {
          case c @ ('"' | '\\') =>
            builder.append('\\'); builder.append(c)
          case '\b' => builder.append('\\'); builder.append('b')
          case '\f' => builder.append('\\'); builder.append('f')
          case '\n' => builder.append('\\'); builder.append('n')
          case '\r' => builder.append('\\'); builder.append('r')
          case '\t' => builder.append('\\'); builder.append('t')
          case c => builder.append(c)
        }
        cs.step()
      }
      builder.append('"')
    }

    private[recon] def writeReconIdent(builder: StringBuilder): Unit = {
      val cs = iterator
      while (!cs.isEmpty) {
        builder.append(cs.head)
        cs.step()
      }
    }

    private[recon] override def writeRecon(builder: StringBuilder, inMarkup: Boolean): Unit =
      if (inMarkup) writeReconMarkup(builder)
      else if (isIdent) writeReconIdent(builder)
      else writeReconString(builder)

    override def writeRecon(builder: StringBuilder): Unit = writeRecon(builder, inMarkup = false)

    override def in(recon: Recon): recon.Text =
      if (recon eq Recon) asInstanceOf[recon.Text]
      else recon.Text(toUString.toString)

    protected override def stringPrefix: String = "Text"
  }

  abstract class ReconTextFactory extends StringFactory[Text] {
    def unapply(text: Text): Maybe[String] = Bind(text.toUString.toString)

    override def toString: String = (String.Builder~Recon.toString~'.'~"Text").state
  }


  trait ReconData extends Equals with Family[Data] with Loader with ReconValue { this: Data =>
    override def isData: Boolean = true
    override def asData: Data = this

    override def writeRecon(builder: StringBuilder): Unit = {
      builder.append('%')
      this.writeBase64(builder)
    }

    override def in(recon: Recon): recon.Data =
      if (recon eq Recon) asInstanceOf[recon.Data]
      else recon.Data.from(this)

    protected override def stringPrefix: String = "Data"
  }

  abstract class ReconDataFactory extends DataFactory[Data] {
    def apply(base64: String): Data = this.fromBase64(base64)

    override def toString: String = (String.Builder~Recon.toString~'.'~"Data").state
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

    override def in(recon: Recon): recon.Number

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

    override def in(recon: Recon): recon.Number =
      if (recon eq Recon) asInstanceOf[recon.Number]
      else recon.Number(toInt)
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

    override def in(recon: Recon): recon.Number =
      if (recon eq Recon) asInstanceOf[recon.Number]
      else recon.Number(toLong)
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

    override def in(recon: Recon): recon.Number =
      if (recon eq Recon) asInstanceOf[recon.Number]
      else recon.Number(toFloat)
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

    override def in(recon: Recon): recon.Number =
      if (recon eq Recon) asInstanceOf[recon.Number]
      else recon.Number(toDouble)
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


  trait ReconExtant extends ReconValue { this: Extant =>
    override def isExtant: Boolean = true
    override def asExtant: Extant = this

    override def writeRecon(builder: StringBuilder): Unit = ()

    override def toRecon: String = ""

    override def in(recon: Recon): recon.Extant = recon.Extant

    override def toString: String = "Extant"
  }


  trait ReconAbsent extends ReconValue { this: Absent =>
    override def isDefined: Boolean = false

    override def isAbsent: Boolean = true
    override def asAbsent: Absent = this

    override def writeRecon(builder: StringBuilder): Unit = ()

    override def toRecon: String = ""

    override def in(recon: Recon): recon.Absent = recon.Absent

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

  private final class BooleanToValue extends AbstractFunction1[Boolean, Value] {
    override def apply(value: Boolean): Value = if (value) True else False
    override def toString: String = (String.Builder~Recon.toString~'.'~"BooleanToBool").state
  }


  class ReconParser extends ReconFactory with recon.ReconParser { ReconParser =>
    private[recon] override type Item = Recon.Item
    private[recon] override type Field = Recon.Field
    private[recon] override type Attr = Recon.Attr
    private[recon] override type Slot = Recon.Slot
    private[recon] override type Value = Recon.Value
    private[recon] override type Record = Recon.Record
    private[recon] override type Text = Recon.Text
    private[recon] override type Data = Recon.Data
    private[recon] override type Number = Recon.Number
    private[recon] override type Extant = Recon.Extant
    private[recon] override type Absent = Recon.Absent

    private[recon] override def Attr(key: Text, value: Value): Attr = Recon.Attr(key, value)
    private[recon] override def Attr(key: Text): Attr = Recon.Attr(key)

    private[recon] override def Slot(key: Value, value: Value): Slot = Recon.Slot(key, value)
    private[recon] override def Slot(key: Value): Slot = Recon.Slot(key)

    private[recon] override def ValueBuilder: ItemBuilder with State[Value] = new ValueBuilder()
    private[recon] override def RecordBuilder: ItemBuilder with State[Record] = new RecordBuilder()
    private[recon] override def TextBuilder: StringBuilder with State[Text] = Recon.TextBuilder
    private[recon] override def DataFramer: Framer with State[Data] = Recon.DataFramer

    private[recon] override def Number(value: String): Number = Recon.Number(value)

    private[recon] override def True: Value = Recon.True
    private[recon] override def False: Value = Recon.False

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
  override type Item   = recon.Item
  override type Field  = recon.Field
  override type Attr   = recon.Attr
  override type Slot   = recon.Slot
  override type Value  = recon.Value
  override type Record = recon.Record
  override type Text   = recon.Text
  override type Data   = recon.Data
  override type Number = recon.Number
  override type Extant = recon.Extant
  override type Absent = recon.Absent

  override val Item   = recon.Item
  override val Field  = recon.Field
  override val Attr   = recon.Attr
  override val Slot   = recon.Slot
  override val Value  = recon.Value
  override val Record = recon.Record
  override val Text   = recon.Text
  override val Data   = recon.Data
  override val Number = recon.Number
  override val True   = new Text(new UString("true"))
  override val False  = new Text(new UString("false"))
  override val Extant = recon.Extant
  override val Absent = recon.Absent

  implicit override lazy val ItemTag: ClassTag[Item] = ClassTag(Predef.classOf[Item])
  implicit override lazy val FieldTag: ClassTag[Field] = ClassTag(Predef.classOf[Field])
  implicit override lazy val AttrTag: ClassTag[Attr] = ClassTag(Predef.classOf[Attr])
  implicit override lazy val SlotTag: ClassTag[Slot] = ClassTag(Predef.classOf[Slot])
  implicit override lazy val ValueTag: ClassTag[Value] = ClassTag(Predef.classOf[Value])
  implicit override lazy val RecordTag: ClassTag[Record] = ClassTag(Predef.classOf[Record])
  implicit override lazy val TextTag: ClassTag[Text] = ClassTag(Predef.classOf[Text])
  implicit override lazy val DataTag: ClassTag[Data] = ClassTag(Predef.classOf[Data])
  implicit override lazy val NumberTag: ClassTag[Number] = ClassTag(Predef.classOf[Number])
  implicit override lazy val ExtantTag: ClassTag[Extant] = ClassTag(Predef.classOf[Extant])
  implicit override lazy val AbsentTag: ClassTag[Absent] = ClassTag(Predef.classOf[Absent])

  override def toString: String = "Recon"
}


sealed abstract class Item extends Recon.ReconItem

object Item extends Recon.ReconItemFactory


sealed abstract class Field extends Item with Recon.ReconField

object Field extends Recon.ReconFieldFactory


final class Attr(
    override val key: Text,
    override val value: Value)
  extends Field with Recon.ReconAttr

object Attr extends Recon.ReconAttrFactory {
  override def apply(key: Text, value: Value): Attr = new Attr(key, value)
}


final class Slot(
    override val key: Value,
    override val value: Value)
  extends Field with Recon.ReconSlot

object Slot extends Recon.ReconSlotFactory {
  override def apply(key: Value, value: Value): Slot = new Slot(key, value)
}


sealed abstract class Value extends Item with Recon.ReconValue

object Value extends Recon.ReconValueFactory


final class Record private[recon] (
    protected val self: FingerTrieSeq[Item],
    private[this] var index: HashTrieMap[Value, Value])
  extends Value with Recon.ReconRecord {

  private[recon] def this(self: FingerTrieSeq[Item]) = this(self, null)

  private[this] def fields: HashTrieMap[Value, Value] = {
    if (index eq null) index = {
      var index = HashTrieMap.empty[Value, Value]
      self.foreach { item =>
        if (item.isField) index += (item.key, item.value)
      }
      index
    }
    index
  }

  override def isEmpty: Boolean = self.isEmpty

  override def length: Int = self.length

  override def contains(key: Value): Boolean =
    if (length > 8) fields.contains(key)
    else self.exists(_.key.equals(key))

  override def apply(index: Int): Item = self(index)

  override def apply(key: Value): Value =
    if (length > 8) fields(key)
    else {
      val these = self.iterator
      while (!these.isEmpty) {
        val item = these.head
        if (item.key.equals(key)) return item.key
        these.step()
      }
      throw new NoSuchElementException(key.toString)
    }

  override def / (key: Value): Value =
    if (length > 8) { if (fields.contains(key)) fields(key) else Absent }
    else {
      val these = self.iterator
      while (!these.isEmpty) {
        val item = these.head
        if (item.key.equals(key)) return item.value
        these.step()
      }
      Absent
    }

  override def head: Item = self.head

  override def foot: Item = self.foot

  override def tail: Record =
    new Record(self.tail, if ((index ne null) && self.head.isValue) index else null)

  override def drop(lower: Int): Record = new Record(self.drop(lower))

  override def take(upper: Int): Record = new Record(self.take(upper))

  override def :+ (item: Item): Record =
    new Record(
      self :+ item,
      if (index ne null) {
        if (item.isField) index + (item.key, item.value)
        else index
      }
      else null)

  override def +: (item: Item): Record =
    new Record(
      item +: self,
      if (index ne null) {
        if (item.isField && !index.contains(item.key)) index + (item.key, item.value)
        else index
      }
      else null)

  override def + (item: Item): Record =
    if (item.isValue)
      new Record(self :+ item, index)
    else if ((index ne null) && !index.contains(item.key))
      new Record(self :+ item, index + (item.key, item.value))
    else {
      var i = length - 1
      val key = item.key
      while (i >= 0 && !self(i).key.equals(key)) i -= 1
      if (i >= 0) {
        if (self(i).value.equals(item.value)) this
        else new Record(self.update(i, item), if (index ne null) index + (key, item.value) else null)
      }
      else new Record(self :+ item, index)
    }

  override def - (key: Value): Record =
    if ((index ne null) && !index.contains(key)) this
    else
      new Record(
        self.filter(!_.key.equals(key))(FingerTrieSeq.Builder),
        if (index ne null) index - key else null)

  override def ++ (that: Record): Record =
    self.++(that.self)(Recon.RecordBuilder)

  override def -- (that: Record): Record =
    self.filter(item => !that.contains(item.key))(Recon.RecordBuilder)

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


final class Data private[recon] (protected val self: Loader) extends Value with Recon.ReconData {
  override def endian: Endianness = self.endian

  override def size: Long = self.size

  override def as[E <: Endianness](endian: E): Data with basis.data.ByteOrder[E] = {
    if (self.endian eq endian) this
    else new Data(self as endian)
  }.asInstanceOf[Data with basis.data.ByteOrder[E]]

  override def loadByte(address: Long): Byte     = self.loadByte(address)
  override def loadShort(address: Long): Short   = self.loadShort(address)
  override def loadInt(address: Long): Int       = self.loadInt(address)
  override def loadLong(address: Long): Long     = self.loadLong(address)
  override def loadFloat(address: Long): Float   = self.loadFloat(address)
  override def loadDouble(address: Long): Double = self.loadDouble(address)

  override def reader(address: Long): Reader = self.reader(address)

  override def toArray: Array[Byte] = self.toArray
}

object Data extends Recon.ReconDataFactory {
  override def endian: Endianness = NativeEndian

  override val empty: Data = new Data(ArrayData.empty)

  override def from(data: Loader): Data =
    if (data.isInstanceOf[Data]) data.asInstanceOf[Data]
    else new Data(data)

  override def Framer: Framer with State[Data] = new DataFramer(ArrayData.Framer)
}


sealed abstract class Number extends Value with Recon.ReconNumber

private[recon] final class IntNumber(override val toInt: Int) extends Number with Recon.ReconInt

private[recon] final class LongNumber(override val toLong: Long) extends Number with Recon.ReconLong

private[recon] final class FloatNumber(override val toFloat: Float) extends Number with Recon.ReconFloat

private[recon] final class DoubleNumber(override val toDouble: Double) extends Number with Recon.ReconDouble

object Number extends Recon.ReconNumberFactory {
  override def apply(value: Int): Number = new IntNumber(value)
  override def apply(value: Long): Number = new LongNumber(value)
  override def apply(value: Float): Number = new FloatNumber(value)
  override def apply(value: Double): Number = new DoubleNumber(value)
}

private[recon] final class DataFramer(self: Framer with State[Loader]) extends Framer with State[Data] {
  override def endian: Endianness = self.endian
  override def isEOF: Boolean = self.isEOF
  override def writeByte(value: Byte): Unit = self.writeByte(value)
  override def writeShort(value: Short): Unit = self.writeShort(value)
  override def writeInt(value: Int): Unit = self.writeInt(value)
  override def writeLong(value: Long): Unit = self.writeLong(value)
  override def writeFloat(value: Float): Unit = self.writeFloat(value)
  override def writeDouble(value: Double): Unit = self.writeDouble(value)
  override def writeData(data: Loader): Unit = self.writeData(data)
  override def clear(): Unit = self.clear()
  override def expect(count: Long): this.type = { self.expect(count); this }
  override def state: Data = new Data(self.state)
  override def toString: String = (String.Builder~Recon.toString~'.'~"Data"~'.'~"Framer").state
}


sealed abstract class Extant extends Value with Recon.ReconExtant

object Extant extends Extant


sealed abstract class Absent extends Value with Recon.ReconAbsent {
  override def isDefined: Boolean = false
}

object Absent extends Absent
