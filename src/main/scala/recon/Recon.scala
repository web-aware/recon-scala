package recon

import basis._
import basis.collections._
import basis.collections.immutable._
import basis.data._
import basis.text._
import basis.util._

sealed abstract class Item extends Comparable[Item] {
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
  def asExtant: Extant.type = throw new MatchError("not Extant")

  def isAbsent: Boolean = false
  def asAbsent: Absent.type = throw new MatchError("not Absent")

  def key: Value
  def value: Value

  def name: String = ""

  def target: Value = value

  def contains(key: Value): Boolean
  def contains(key: String): Boolean
  def contains(index: Int): Boolean

  def apply(key: Value): Value
  def apply(key: String): Value
  def apply(index: Int): Item

  def / (key: Value): Value
  def / (key: String): Value
  def / (index: Int): Item

  def head: Item = Absent
  def foot: Item = Absent

  def cast[T](implicit form: Form[T]): Maybe[T]

  def coerce[@specialized(Form.Specialized) T](implicit form: Form[T]): T

  def asString: String = throw new MatchError("not Text")

  def writeRecon(builder: StringBuilder): Unit

  def writeReconBlock(builder: StringBuilder): Unit = writeRecon(builder)

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

object Item extends java.util.Comparator[Item] {
  override def compare(x: Item, y: Item): Int = x.compareTo(y)
}


sealed abstract class Field extends Item {
  override def isField: Boolean = true
  override def asField: Field = this

  override def contains(key: Value): Boolean = value.contains(key)
  override def contains(key: String): Boolean = value.contains(key)
  override def contains(index: Int): Boolean = value.contains(index)

  override def apply(key: Value): Value = value(key)
  override def apply(key: String): Value = value(key)
  override def apply(index: Int): Item = value(index)

  override def / (key: Value): Value = value / key
  override def / (key: String): Value = value / key
  override def / (index: Int): Item = value / index

  override def cast[T](implicit form: Form[T]): Maybe[T] =
    value.cast[T](form)

  override def coerce[@specialized(Form.Specialized) T](implicit form: Form[T]): T =
    value.coerce[T](form)
}

object Field {
  def unapply(field: Field): Maybe[(Value, Value)] = Bind((field.key, field.value))

  override def toString: String = "Field"
}


final class Attr(override val key: Text, override val value: Value) extends Field {
  override def isAttr: Boolean = true
  override def asAttr: Attr = this

  override def name: String = key.asString

  override def writeRecon(builder: StringBuilder): Unit = {
    builder.append('@')
    key.writeRecon(builder)
    if (!value.isExtant) {
      builder.append('(')
      value.writeReconBlock(builder)
      builder.append(')')
    }
  }

  override def compareTo(other: Item): Int = {
    if (other.isInstanceOf[Attr]) compareTo(other.asInstanceOf[Attr])
    else -1
  }

  private[this] def compareTo(that: Attr): Int = {
    val keyOrder = key.compareTo(that.key)
    if (keyOrder != 0) keyOrder
    else value.compareTo(that.value)
  }

  override def equals(other: Any): Boolean =
    eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Attr] && {
      val that = other.asInstanceOf[Attr]
      key.equals(that.key) && value.equals(that.value)
    }

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(mix(seed[Attr], key.hashCode), value.hashCode))
  }

  override def toString: String = {
    val s = String.Builder~"Attr"~'('~>key.asString
    if (!value.isExtant) s~", "~>value
    (s~')').state
  }
}

object Attr {
  def apply(key: Text, value: Value): Attr = new Attr(key, value)

  def apply(key: Text): Attr = new Attr(key, Extant)

  def apply(key: String, value: Value): Attr = new Attr(new Text(new UString(key)), value)

  def apply(key: String): Attr = new Attr(new Text(new UString(key)), Extant)

  def unapply(attr: Attr): Maybe[(Text, Value)] = Bind((attr.key, attr.value))

  override def toString: String = "Attr"
}


final class Slot(override val key: Value, override val value: Value) extends Field {
  override def isSlot: Boolean = true
  override def asSlot: Slot = this

  override def name: String = if (key.isText) key.asString else ""

  override def writeRecon(builder: StringBuilder): Unit = {
    key.writeRecon(builder)
    builder.append(':')
    if (!value.isExtant) value.writeRecon(builder)
  }

  override def compareTo(other: Item): Int = {
    if (other.isInstanceOf[Slot]) compareTo(other.asInstanceOf[Slot])
    else if (other.isInstanceOf[Attr]) 1
    else -1
  }

  private[this] def compareTo(that: Slot): Int = {
    val keyOrder = key.compareTo(that.key)
    if (keyOrder != 0) keyOrder
    else value.compareTo(that.value)
  }

  override def equals(other: Any): Boolean =
    eq(other.asInstanceOf[AnyRef]) || other.isInstanceOf[Slot] && {
      val that = other.asInstanceOf[Slot]
      key.equals(that.key) && value.equals(that.value)
    }

  override def hashCode: Int = {
    import MurmurHash3._
    mash(mix(mix(seed[Slot], key.hashCode), value.hashCode))
  }

  override def toString: String = {
    val s = String.Builder~"Slot"~'('~>(if (key.isText) key.asString else key)
    if (!value.isExtant) s~", "~>value
    (s~')').state
  }
}

object Slot {
  def apply(key: Value, value: Value): Slot = new Slot(key, value)

  def apply(key: Value): Slot = new Slot(key, Extant)

  def apply(key: String, value: Value): Slot = new Slot(new Text(new UString(key)), value)

  def apply(key: String): Slot = new Slot(new Text(new UString(key)), Extant)

  def unapply(slot: Slot): Maybe[(Value, Value)] = Bind((slot.key, slot.value))

  override def toString: String = "Slot"
}


sealed abstract class Value extends Item {
  override def isValue: Boolean = true
  override def asValue: Value = this

  override def key: Value = Absent
  override def value: Value = this

  override def contains(key: Value): Boolean = false
  override def contains(key: String): Boolean = false
  override def contains(index: Int): Boolean = false

  override def apply(key: Value): Value = throw new NoSuchElementException(key.toString)
  override def apply(key: String): Value = throw new NoSuchElementException(key)
  override def apply(index: Int): Item = throw new NoSuchElementException(key.toString)

  override def / (key: Value): Value = Absent
  override def / (key: String): Value = Absent
  override def / (index: Int): Item = Absent

  override def cast[T](implicit form: Form[T]): Maybe[T] =
    form.cast(this)

  override def coerce[@specialized(Form.Specialized) T](implicit form: Form[T]): T =
    form.cast(this).bindOrElse(form.unit)
}

object Value {
  def undefined: Value = Absent

  def apply[@specialized(Form.Specialized) T](value: T)(implicit form: Form[T]): Value =
    form.mold(value)

  def parseRecon(recon: String): Value = {
    val result = ReconParser.BlockParser.run(new UString(recon).iterator)
    if (result.isDone) result.bind
    else result.trap match {
      case ex: Throwable => throw ex
      case error => throw new ReconException(error.toString)
    }
  }

  implicit def Builder: Builder[Item] with State[Value] = new ValueBuilder()

  override def toString: String = "Value"
}

private[recon] final class ValueBuilder extends Builder[Item] with State[Value] {
  private[this] var builder: Builder[Item] with basis.State[Record] = null
  private[this] var value: Value = null.asInstanceOf[Value]

  override def append(item: Item): Unit = {
    if (builder ne null) builder.append(item)
    else if ((value eq null) && item.isValue) value = item.asValue
    else {
      builder = Record.Builder
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

  override def toString: String = (String.Builder~"Value"~'.'~"Builder").state
}


final class Record private[recon] (
    protected val self: FingerTrieSeq[Item],
    private[this] var index: HashTrieMap[AnyRef, Value])
  extends Value
  with Equals
  with Immutable
  with Family[Record]
  with IndexedSeq[Item] {

  private[recon] def this(self: FingerTrieSeq[Item]) = this(self, null)

  private[this] def fields: HashTrieMap[AnyRef, Value] = {
    if (index eq null) index = {
      var index = HashTrieMap.empty[AnyRef, Value]
      self.foreach { item =>
        if (item.isField) {
          val field = item.asField
          index += (field.key, field.value)
          if (field.key.isText) index += (field.name, field.value)
        }
      }
      index
    }
    index
  }

  override def isRecord: Boolean = true
  override def asRecord: Record = this

  override def isEmpty: Boolean = self.isEmpty

  override def length: Int = self.length

  override def contains(key: Value): Boolean =
    if (length > 8) fields.contains(key)
    else self.exists { item =>
      key.equals(item.key)
    }

  override def contains(key: String): Boolean =
    if (length > 8) fields.contains(key)
    else self.exists { item =>
      item.key.isText && key.equals(item.name)
    }

  override def contains(index: Int): Boolean =
    0 <= index && index < self.length

  override def apply(key: Value): Value =
    if (length > 8) fields(key)
    else {
      val these = self.iterator
      while (!these.isEmpty) {
        val item = these.head
        if (key.equals(item.key)) return item.value
        these.step()
      }
      throw new NoSuchElementException(key.toString)
    }

  override def apply(key: String): Value =
    if (length > 8) fields(key)
    else {
      val these = self.iterator
      while (!these.isEmpty) {
        val item = these.head
        if (item.key.isText && key.equals(item.name)) return item.value
        these.step()
      }
      throw new NoSuchElementException(key)
    }

  override def apply(index: Int): Item = self(index)

  override def / (key: Value): Value =
    if (length > 8) { if (fields.contains(key)) fields(key) else Absent }
    else {
      val these = self.iterator
      while (!these.isEmpty) {
        val item = these.head
        if (key.equals(item.key)) return item.value
        these.step()
      }
      Absent
    }

  override def / (key: String): Value =
    if (length > 8) { if (fields.contains(key)) fields(key) else Absent }
    else {
      val these = self.iterator
      while (!these.isEmpty) {
        val item = these.head
        if (item.key.isText && key.equals(item.name)) return item.value
        these.step()
      }
      Absent
    }

  override def / (index: Int): Item =
    if (0 <= index && index < self.length) self(index) else Absent

  override def target: Value = {
    val items = iterator
    while (!items.isEmpty) {
      val item = items.head
      if (item.isValue) return item.asValue
      items.step()
    }
    Absent
  }

  override def head: Item = if (!self.isEmpty) self.head else Absent

  def tail: Record = new Record(self.tail, if ((index ne null) && self.head.isValue) index else null)

  def body: Record = new Record(self.body, null)

  override def foot: Item = if (!self.isEmpty) self.foot else Absent

  def drop(lower: Int): Record = new Record(self.drop(lower))

  def take(upper: Int): Record = new Record(self.take(upper))

  def slice(lower: Int, upper: Int): Record = new Record(self.slice(lower, upper))

  def :+ (item: Item): Record =
    new Record(
      self :+ item,
      if (index ne null) {
        if (item.isField) index + (item.key, item.value)
        else index
      }
      else null)

  def +: (item: Item): Record =
    new Record(
      item +: self,
      if (index ne null) {
        if (item.isField && !index.contains(item.key)) index + (item.key, item.value)
        else index
      }
      else null)

  def + (item: Item): Record =
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

  def - (key: Value): Record =
    if ((index ne null) && !index.contains(key)) this
    else
      new Record(
        self.filter(item => key.equals(item.key))(FingerTrieSeq.Builder),
        if (index ne null) {
          if (key.isText) index - key - key.asString
          else index - key
        }
        else null)

  def ++ (that: Record): Record =
    self.++(that.self)(Record.Builder)

  def -- (that: Record): Record =
    self.filter(item => !that.contains(item.key))(Record.Builder)

  override def iterator: Iterator[Item] = self.iterator

  override def traverse(f: Item => Unit): Unit = self.traverse(f)

  private def isBlockSafe: Boolean = self.forall(!_.isAttr)

  private def isMarkupSafe: Boolean = !isEmpty && head.isAttr && tail.forall(!_.isAttr)

  override def writeRecon(builder: StringBuilder): Unit =
    if (!isEmpty) writeReconRecord(builder, inBlock = false, inMarkup = false)
    else {
      builder.append('{')
      builder.append('}')
    }

  override def writeReconBlock(builder: StringBuilder): Unit =
    if (!isEmpty) writeReconRecord(builder, inBlock = isBlockSafe, inMarkup = false)
    else {
      builder.append('{')
      builder.append('}')
    }

  private def writeReconRecord(builder: StringBuilder, inBlock: Boolean, inMarkup: Boolean): Unit = {
    val items = iterator
    var inBraces = false
    var inBrackets = false
    var first = true
    while (!items.isEmpty) {
      val item = items.head
      items.step()
      if (inBrackets && item.isAttr) {
        if (inBraces) {
          builder.append('}')
          inBraces = false
        }
        builder.append(']')
        inBrackets = false
      }
      if (item.isAttr) {
        if (inBraces) {
          builder.append('}')
          inBraces = false
        }
        else if (inBrackets) {
          builder.append(']')
          inBrackets = false
        }
        item.writeRecon(builder)
        first = false
      }
      else if (inBrackets && item.isText) {
        if (inBraces) {
          builder.append('}')
          inBraces = false
        }
        item.asText.writeReconMarkupText(builder)
      }
      else if (inBraces) {
        if (!first) builder.append(',')
        else first = false
        writeReconItem(item, builder)
      }
      else if (inBrackets) {
        if (item.isRecord && item.asRecord.isMarkupSafe) {
          item.asRecord.writeReconRecord(builder, inBlock = false, inMarkup = true)
          if (!items.isEmpty && items.head.isText) {
            items.head.asText.writeReconMarkupText(builder)
            items.step()
          }
          else if (!items.isEmpty && !items.head.isAttr) {
            builder.append('{')
            inBraces = true
            first = true
          }
          else {
            builder.append(']')
            inBrackets = false
          }
        }
        else {
          builder.append('{')
          item.writeRecon(builder)
          inBraces = true
          first = false
        }
      }
      else if (item.isText && !items.isEmpty && !items.head.isField && !items.head.isText) {
        builder.append('[')
        item.asText.writeReconMarkupText(builder)
        inBrackets = true
      }
      else if (inBlock && !inBraces) {
        if (!first) builder.append(',')
        else first = false
        writeReconItem(item, builder)
      }
      else if (inMarkup && item.isText && items.isEmpty) {
        builder.append('[')
        item.asText.writeReconMarkupText(builder)
        builder.append(']')
      }
      else if (!inMarkup && item.isValue && !item.isRecord &&
              (!first && items.isEmpty || !items.isEmpty && items.head.isAttr)) {
        if (!first && (item.isText && item.asText.isIdent || item.isNumber)) builder.append(' ')
        item.writeRecon(builder)
      }
      else {
        builder.append('{')
        item.writeRecon(builder)
        inBraces = true
        first = false
      }
    }
    if (inBraces) builder.append('}')
    else if (inBrackets) builder.append(']')
  }

  private def writeReconItem(item: Item, builder: StringBuilder): Unit = {
    if (item.isField) {
      item.key.writeRecon(builder)
      builder.append(':')
      if (!item.value.isExtant) item.value.writeRecon(builder)
    }
    else item.writeRecon(builder)
  }

  def valuesIterator: Iterator[Value] = new RecordValuesIterator(iterator)

  override def compareTo(other: Item): Int = {
    if (other.isInstanceOf[Record]) compareTo(other.asInstanceOf[Record])
    else if (other.isInstanceOf[Field]) 1
    else -1
  }

  private[this] def compareTo(that: Record): Int = {
    val p = length
    val q = that.length
    val n = p min q
    var i = 0
    var itemOrder = 0
    while (i < n && itemOrder == 0) {
      itemOrder = this(i).compareTo(that(i))
      i += 1
    }
    if (itemOrder != 0) itemOrder
    else if (p > q) 1
    else if (p < q) -1
    else 0
  }

  protected override def stringPrefix: String = "Record"
}

object Record extends special.SeqSource[Record, Item] {
  override val empty: Record = new Record(FingerTrieSeq.empty)

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

  @noinline implicit override def Builder: Builder[Item] with State[Record] =
    new RecordBuilder(FingerTrieSeq.Builder)

  override def toString: String = "Record"
}

private[recon] final class RecordValuesIterator(self: Iterator[Item]) extends Iterator[Value] {
  override def isEmpty: Boolean = self.isEmpty

  override def head: Value = self.head.value

  override def step(): Unit = self.step()

  override def dup: Iterator[Value] = new RecordValuesIterator(self.dup)
}

private[recon] final class RecordBuilder(self: Builder[Item] with State[FingerTrieSeq[Item]])
  extends Builder[Item] with State[Record] {
  override def append(item: Item): Unit = self.append(item)
  override def clear(): Unit = self.clear()
  override def expect(count: Int): this.type = { self.expect(count); this }
  override def state: Record = new Record(self.state)
  override def toString: String = (String.Builder~"Record"~'.'~"Builder").state
}


final class Text private[recon] (protected val self: UString)
  extends Value
  with Equals
  with Family[Text]
  with UTF {

  override def isText: Boolean = true
  override def asText: Text = this

  private[this] var utf8Size: Int = -1
  override def utf8Length: Int = {
    if (utf8Size == -1) utf8Size = super.utf8Length
    utf8Size
  }

  def isIdent: Boolean = {
    val cs = iterator
    !cs.isEmpty && ReconParser.isNameStartChar(cs.head) && {
      cs.step()
      while (!cs.isEmpty && ReconParser.isNameChar(cs.head)) cs.step()
      cs.isEmpty
    }
  }

  def isBlank: Boolean = this.forall { c => c == 0x20 || c == 0x9 }

  private[recon] def writeReconMarkupText(builder: StringBuilder): Unit = {
    val cs = iterator
    while (!cs.isEmpty) {
      cs.head match {
        case c @ ('@' | '[' | '\\' | ']' | '{' | '}') =>
          builder.append('\\'); builder.append(c)
        case c => builder.append(c)
      }
      cs.step()
    }
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

  override def writeRecon(builder: StringBuilder): Unit =
    if (isIdent) writeReconIdent(builder)
    else writeReconString(builder)

  override def iterator: Iterator[Int] = self.iterator

  override def asString: String = self.toString

  override def compareTo(other: Item): Int = {
    if (other.isInstanceOf[Text]) asString.compareTo(other.asInstanceOf[Text].asString)
    else if (other.isInstanceOf[Field] ||
             other.isInstanceOf[Record] ||
             other.isInstanceOf[Data]) 1
    else -1
  }

  protected override def stringPrefix: String = "Text"
}

object Text extends StringFactory[Text] {
  override val empty: Text = new Text(new UString(""))

  override def apply(chars: CharSequence): Text = new Text(new UString(chars.toString))

  def unapply(text: Text): Maybe[String] = Bind(text.asString)

  @noinline implicit override def Builder: StringBuilder with State[Text] = new TextBuilder(UString.Builder)

  override def toString: String = "Text"
}

private[recon] final class TextBuilder(self: StringBuilder with State[UString])
  extends StringBuilder with State[Text] {
  override def append(c: Int): Unit = self.append(c)
  override def append(cs: CharSequence): Unit = self.append(cs)
  override def clear(): Unit = self.clear()
  override def expect(count: Int): this.type = { self.expect(count); this }
  override def state: Text = new Text(self.state)
  override def toString: String = (String.Builder~"Text"~'.'~"Builder").state
}


final class Data private[recon] (protected val self: Loader)
  extends Value
  with Equals
  with Family[Data]
  with Loader {

  override def isData: Boolean = true
  override def asData: Data = this

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

  override def writeRecon(builder: StringBuilder): Unit = {
    builder.append('%')
    this.writeBase64(builder)
  }

  override def compareTo(other: Item): Int = {
    if (other.isInstanceOf[Data]) compareTo(other.asInstanceOf[Data])
    else if (other.isInstanceOf[Field] || other.isInstanceOf[Record]) 1
    else -1
  }

  private[this] def compareTo(that: Data): Int = {
    val p = size
    val q = that.size
    val n = p min q
    var i = 0L
    var byteOrder = 0
    while (i < n && byteOrder == 0) {
      byteOrder = loadByte(i) - that.loadByte(i)
      i += 1L
    }
    if (byteOrder > 0) 1
    else if (byteOrder < 0) -1
    else if (p > q) 1
    else if (p < q) -1
    else 0
  }

  protected override def stringPrefix: String = "Data"
}

object Data extends DataFactory[Data] {
  override def endian: Endianness = NativeEndian

  override val empty: Data = new Data(ArrayData.empty)

  def apply(base64: String): Data = this.fromBase64(base64)

  override def from(data: Loader): Data =
    if (data.isInstanceOf[Data]) data.asInstanceOf[Data]
    else new Data(data)

  @noinline implicit override def Framer: Framer with State[Data] = new DataFramer(ArrayData.Framer)

  override def toString: String = "Data"
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
  override def toString: String = (String.Builder~"Data"~'.'~"Framer").state
}


sealed abstract class Number extends Value {
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

  override def compareTo(other: Item): Int = {
    if (other.isInstanceOf[Number])
      java.lang.Double.compare(toDouble, other.asInstanceOf[Number].toDouble)
    else if (other.isInstanceOf[Field] ||
             other.isInstanceOf[Record] ||
             other.isInstanceOf[Data] ||
             other.isInstanceOf[Text]) 1
    else -1
  }

  override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
    case that: Number =>
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

private[recon] final class IntNumber(override val toInt: Int) extends Number {
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

private[recon] final class LongNumber(override val toLong: Long) extends Number {
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

private[recon] final class FloatNumber(override val toFloat: Float) extends Number {
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

private[recon] final class DoubleNumber(override val toDouble: Double) extends Number {
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

object Number {
  def apply(value: Int): Number = new IntNumber(value)
  def apply(value: Long): Number = new LongNumber(value)
  def apply(value: Float): Number = new FloatNumber(value)
  def apply(value: Double): Number = new DoubleNumber(value)

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

  override def toString: String = "Number"
}


object Extant extends Value {
  override def isExtant: Boolean = true
  override def asExtant: Extant.type = this

  override def writeRecon(builder: StringBuilder): Unit = ()

  override def toRecon: String = ""

  override def compareTo(other: Item): Int = {
    if (eq(other)) 0
    else if (other.isInstanceOf[Field] ||
             other.isInstanceOf[Record] ||
             other.isInstanceOf[Data] ||
             other.isInstanceOf[Text] ||
             other.isInstanceOf[Number]) 1
    else -1
  }

  override def toString: String = "Extant"
}


object Absent extends Value {
  override def isDefined: Boolean = false

  override def isAbsent: Boolean = true
  override def asAbsent: Absent.type = this

  override def writeRecon(builder: StringBuilder): Unit = ()

  override def toRecon: String = ""

  override def compareTo(other: Item): Int = {
    if (eq(other)) 0
    else 1
  }

  override def toString: String = "Absent"
}
